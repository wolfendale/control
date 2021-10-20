package wolfendale.control

import cats._
import cats.data._
import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import scala.util.control.NoStackTrace

sealed abstract class Journey[+A] {

  import Journey._

  def page: Option[Page]

  final def flatMap[B](f: A => Journey[B]): Journey[B] =
    this match {
      case r: Finished[A]   => Continue(r, f, r.page)
      case c: Continue[A] => Continue(c.nested, (x: c.X) => c.f(x).flatMap(f), c.page)
    }

  final def map[B](f: A => B): Journey[B] =
    flatMap(a => pure(f(a)))

  // TODO make better? D:
  final def handleErrorWith[B >: A](f: Throwable => Journey[B])(implicit ec: ExecutionContext): Journey[B] =
    this match {
      case p: Pure[A]        => p
      case Suspend(fa, key)  => roll(fa.map(pure).recoverWith(e => Future.successful(f(e))), key)
      // TODO remove step as this executes the program while building it up
      case c: Continue[A] => roll(c.step.map(_.handleErrorWith(f)).recoverWith(e => f(e).step), c.page)
    }

  final def step(implicit ev: ExecutionContext): Future[Journey[A]] =
    this match {
      case r: Finished[A]   => r.resolve.map(Pure(_))
      case c: Continue[A] => c.continue
    }

  private def run[B](machine: EitherT[WriterT[StateT[Future, Journey[_], *], Vector[Journey[_]], *], Throwable, B])(implicit ec: ExecutionContext): Future[Step[B]] =
    machine.value.run.run(this).map({ case (current, (history, result)) => Step(current, history, result) })

  final def to[P <: Page : ClassTag](id: Journey.Id)(implicit ec: ExecutionContext, defaultsTo: P DefaultsTo Page): Future[Step[P]] =
    run(Journey.runTo[P](id))

  final def toPage(id: Journey.Id)(implicit ec: ExecutionContext): Future[Step[Page]] =
    run(Journey.runTo[Page](id))

  final def nextFor(id: Journey.Id)(implicit ec: ExecutionContext): Future[Step[Id]] =
    run {
      for {
        _    <- Journey.runTo[Page](id)
        _    <- Journey.step
        next <- Journey.resume
      } yield next
    }
}

object Journey {

  sealed abstract class Finished[+A] extends Journey[A] {

    @inline
    final def resolve: Future[A] =
      this match {
        case Pure(a, _)     => Future.successful(a)
        case Suspend(fa, _) => fa
      }
  }

  final case class Pure[+A](value: A, page: Option[Page] = None) extends Finished[A]

  final case class Suspend[+A](value: Future[A], page: Option[Page] = None) extends Finished[A]

  sealed abstract class Continue[+A] extends Journey[A] {

    type X
    val nested: Finished[X]
    val f: X => Journey[A]

    final def continue(implicit ev: ExecutionContext): Future[Journey[A]] =
      nested match {
        case Pure(a, _)     => Future.successful(f(a))
        case Suspend(fa, _) => fa.map(f)
      }

    final override def toString: String =
      s"Continue(..., $page)"
  }

  object Continue {

    def apply[M, A, X0](nested0: Finished[X0], f0: X0 => Journey[A], key0: Option[Page] = None): Continue[A] =
      new Continue[A] {
        override type X = X0
        override val nested: Finished[X0] = nested0
        override val f: X0 => Journey[A] = f0
        override val page: Option[Page] = key0
      }
  }

  def apply[A](fa: Future[A], page: Option[Page] = None): Journey[A] = Suspend(fa, page)

  def pure[A](a: A): Journey[A] = Pure(a)

  def roll[A](fj: Future[Journey[A]], page: Option[Page]): Journey[A] =
    Continue(Suspend(fj), identity[Journey[A]], page)

  def page(page: Page): Journey[Unit] = Journey(Future.unit, Some(page))

  trait Id

  trait Page {
    def id: Id
  }

  object Page {
    def apply(identifier: Id): Page =
      new Page { val id: Id = identifier }
  }

  final case class Step[A](current: Journey[_], history: Vector[Journey[_]], result: Either[Throwable, A]) {
    def lastPage: Option[Page] = current.page.orElse(history.flatMap(_.page).lastOption)
  }

  case object UnexpectedEndError extends Throwable with NoStackTrace {
    override def getMessage: String = "Unexpectedly reached the end of the journey"
  }

  private def machine[A](
                          onComplete: Option[Page] => Future[A],
                          onContinue: Option[Page] => Future[Option[A]],
                          onError: PartialFunction[(Throwable, Option[Page]), Future[A]] = PartialFunction.empty
                        )(implicit ev: ExecutionContext): EitherT[WriterT[StateT[Future, Journey[_], *], Vector[Journey[_]], *], Throwable, Option[A]] = {

    EitherT {
      WriterT {
        StateT {
          case p @ Pure(_, _) =>
            onComplete(p.page).map(b => (p, (Vector(p), b.some.asRight[Throwable])))
          case j1: Journey[_] =>
            onContinue(j1.page).flatMap { ob =>
              ob.fold {
                j1.step.map { j2 =>
                  (j2, (Vector(j1), Option.empty[A].asRight[Throwable]))
                }
              } { b =>
                Future.successful((j1, (Vector.empty, b.some.asRight[Throwable])))
              }
            }.recoverWith { case e =>
              onError.lift((e, j1.page)).fold {
                Future.successful((j1, (Vector.empty, e.asLeft[Option[A]])))
              } {
                _.map { b =>
                  (j1, (Vector.empty, b.some.asRight))
                }
              }
            }
        }
      }
    }
  }

  private def step(implicit ec: ExecutionContext) =
    machine[Unit](
      onComplete = _ => Future.successful(()),
      onContinue = _ => Future.successful(None)
    ).as(())

  private def runWhileM[P <: Page : ClassTag](f: P => Future[Boolean])(implicit ec: ExecutionContext) =
    machine[P](
      onComplete = {
        case Some(page: P) => f(page).ifM(Future.successful(page), Future.failed(UnexpectedEndError))
        case _             => Future.failed(UnexpectedEndError)
      },
      onContinue = {
        case Some(page: P) => f(page).ifM(Future.successful(None), Future.successful(Some(page)))
        case _             => Future.successful(None)
      }
    ).untilDefinedM

  private def runUntilM[P <: Page : ClassTag](f: P => Future[Boolean])(implicit ec: ExecutionContext) =
    runWhileM[P](f(_).map(!_))

  private def runUntil[P <: Page : ClassTag](f: P => Boolean)(implicit ec: ExecutionContext) =
    runUntilM[P](page => Future.successful(f(page)))

  private def runTo[P <: Page : ClassTag](id: Journey.Id)(implicit ec: ExecutionContext) =
    runUntil[P](page => page.id == id)

  private def resume(implicit ec: ExecutionContext) =
    machine[Id](
      onComplete = page => page.map(p => Future.successful(p.id)).getOrElse(Future.failed(UnexpectedEndError)),
      onContinue = page => Future.successful(page.map(_.id))
    ).untilDefinedM

  implicit def monadInstance(implicit ec: ExecutionContext): StackSafeMonad[Journey] with MonadThrow[Journey] =
    new StackSafeMonad[Journey] with MonadThrow[Journey] {
      override def pure[A](x: A): Journey[A] = Journey.pure(x)
      override def flatMap[A, B](fa: Journey[A])(f: A => Journey[B]): Journey[B] = fa.flatMap(f)
      override def raiseError[A](e: Throwable): Journey[A] = Journey(Future.failed(e))
      override def handleErrorWith[A](fa: Journey[A])(f: Throwable => Journey[A]): Journey[A] = fa.handleErrorWith(f)
    }
}