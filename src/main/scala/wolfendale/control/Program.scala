package wolfendale.control

import cats._
import cats.data._
import cats.implicits._
import io.iteratee.Enumerator
import wolfendale.control.machine.Trace

import scala.language.{existentials, implicitConversions}

sealed abstract class Program[F[_], T, E, A] extends Serializable {

  import Program._

  def annotation: Option[T]

  @inline
  final def flatMap[B](f: A => Program[F, T, E, B]): Program[F, T, E, B] =
    this match {
      case e: Error[F, T, E, A]    => e.shift[B]
      case c: Continue[F, T, E, A] => Continue(c.nested, (Kleisli(c.f) >>> Kleisli(f)).run, c.annotation)
      case r: Result[F, T, E, A]   => Continue(r, f, r.annotation)
    }

  @inline
  final def map[B](f: A => B): Program[F, T, E, B] =
    flatMap(a => Pure(f(a)))

  final def foldLeft[B](b: B)(f: (B, A) => B)(implicit F: Functor[F], FF: Foldable[F]): B =
    this match {
      case _: Error[F, T, E, A]    => b
      case Pure(a, _)              => f(b, a)
      case Suspend(fa, _)          => fa.foldLeft(b)(f)
      case c: Continue[F, T, E, A] => c.nested match {
        case Pure(cx, _)            => c.f(cx).foldLeft(b)(f)
        case Suspend(fcx, _)        => fcx.foldLeft(b)((bb, cx) => c.f(cx).foldLeft(bb)(f))
      }
    }

  final def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Functor[F], FF: Foldable[F]): Eval[B] =
    this match {
      case _: Error[F, T, E, A]    => lb
      case Pure(a, _)              => f(a, lb)
      case Suspend(fa, _)          => fa.foldRight(lb)(f)
      case c: Continue[F, T, E, A] => c.nested match {
        case Pure(cx, _)            => c.f(cx).foldRight(lb)(f)
        case Suspend(fcx, _)        => fcx.foldRight(lb)((cx, bb) => c.f(cx).foldRight(bb)(f))
      }
    }

  final def traverse[G[_], B](f: A => G[B])(implicit F: Functor[F], AG: Applicative[G], TF: Traverse[F]): G[Program[F, T, E, B]] =
    this match {
      case e: Error[F, T, E, A]    => AG.pure(e.shift[B])
      case Pure(a, t)              => f(a).map(Pure(_, t))
      case Suspend(fa, t)          => fa.traverse(f).map(Suspend(_, t))
      case c: Continue[F, T, E, A] => c.nested match {
        case Pure(cx, _)            => c.f(cx).traverse(f)
        case Suspend(fcx, _)        => fcx.traverse(cx => c.f(cx).traverse(f)).map(roll)
      }
    }

  final def step(implicit A: ApplicativeError[F, E]): F[Step[F, T, E, A]] =
    this match {
      case e: Error[F, T, E, A]    => A.pure(Step.Halt(e))
      case Pure(a, _)              => A.pure(Step.Result(a))
      case Suspend(fa, _)          => fa.map(a => Step.Result(a))
      case c: Continue[F, T, E, A] => c.continue.redeem(e => Step.Halt(Error(e, c.annotation)), Step.Continue(_))
    }

  final def enumerate(implicit M: MonadError[F, E]): Enumerator[F, Step[F, T, E, A]] = {
    def step(next: Step[F, T, E, A]): F[Option[Step[F, T, E, A]]] =
      next match {
        case Step.Continue(sub) => sub.step.map(Some(_))
        case _                  => M.pure(None)
      }
    Enumerator.iterateUntilM[F, Step[F, T, E, A]](Step.Continue(this))(step)
  }
}

object Program {

  sealed abstract class Result[F[_], T, E, A] extends Program[F, T, E, A] with Serializable

  final case class Pure[F[_], T, E, A](a: A, annotation: Option[T] = None) extends Result[F, T, E, A] {
    override def toString: String =
      s"pure${annotation.map(a => s" @@ $a").mkString}"
  }

  final case class Suspend[F[_], T, E, A](fa: F[A], annotation: Option[T] = None) extends Result[F, T, E, A] {
    override def toString: String =
      s"suspend${annotation.map(a => s" @@ $a").mkString}"
  }

  final case class Error[F[_], T, E, A](error: E, annotation: Option[T] = None) extends Program[F, T, E, A] {
    def shift[B]: Error[F, T, E, B] = this.asInstanceOf[Error[F, T, E, B]]
  }

  abstract class Continue[F[_], T, E, B] extends Program[F, T, E, B] with Serializable {

    type X
    val nested: Result[F, T, E, X]
    val f: X => Program[F, T, E, B]

    @inline
    final def continue(implicit A: Applicative[F]): F[Program[F, T, E, B]] = nested match {
      case Pure(a, _)           => A.pure(f(a))
      case Suspend(fa, _)       => fa.map(f)
    }

    override def toString: String =
      s"continue${annotation.map(a => s" @@ $a").mkString}"
  }

  object Continue {

    type Aux[F[_], T, E, A, B] = Continue[F, T, E, B] { type X = A }

    def apply[F[_], T, E, A, B](pfa: Result[F, T, E, A], g: A => Program[F, T, E, B], a: Option[T] = None): Aux[F, T, E, A, B] =
      new Continue[F, T, E, B] {
        override type X = A
        override val nested: Result[F, T, E, A] = pfa
        override val f: A => Program[F, T, E, B] = g
        override val annotation: Option[T] = a
      }
  }

  @inline
  def pure[F[_], T, E, A](a: A): Program[F, T, E, A] =
    Pure(a)

  @inline
  def suspend[F[_], T, E, A](fa: F[A]): Program[F, T, E, A] =
    Suspend(fa)

  @inline def raiseError[F[_], T, E, A](e: E): Program[F, T, E, A] =
    Error(e)

  @inline
  def roll[F[_], T, E, A](fp: F[Program[F, T, E, A]]): Program[F, T, E, A] =
    suspend(fp).flatMap(identity) // Continue(Suspend, identity)

  sealed abstract class Step[F[_], T, E, A] extends Serializable

  object Step {

    final case class Continue[F[_], T, E, A](program: Program[F, T, E, A]) extends Step[F, T, E, A]
    final case class Halt[F[_], T, E, A](program: Program[F, T, E, A]) extends Step[F, T, E, A]
    final case class Result[F[_], T, E, A](result: A) extends Step[F, T, E, A]
  }

  implicit def monadInstance[F[_], T, E]: Monad[Program[F, T, E, *]] =
    new Monad[Program[F, T, E, *]] with StackSafeMonad[Program[F, T, E, *]] {

      override def pure[A](a: A): Program[F, T, E, A] =
        Program.pure(a)

      override def flatMap[A, B](fa: Program[F, T, E, A])(f: A => Program[F, T, E, B]): Program[F, T, E, B] =
        fa.flatMap(f)

      override def map[A, B](fa: Program[F, T, E, A])(f: A => B): Program[F, T, E, B] =
        fa.map(f)
    }

  implicit def traverseInstance[F[_], T, E](implicit TF: Traverse[F], FF: Foldable[F]): Traverse[Program[F, T, E, *]] =
    new Traverse[Program[F, T, E, *]] {

      override def traverse[G[_], A, B](fa: Program[F, T, E, A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[Program[F, T, E, B]] =
        fa.traverse(f)

      override def foldLeft[A, B](fa: Program[F, T, E, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: Program[F, T, E, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)
    }

  implicit def monadErrorInstance[F[_], T, E](implicit E: MonadError[F, E]): MonadError[Program[F, T, E, *], E] =
    new MonadError[Program[F, T, E, *], E] with StackSafeMonad[Program[F, T, E, *]] {

      override def flatMap[A, B](fa: Program[F, T, E, A])(f: A => Program[F, T, E, B]): Program[F, T, E, B] =
        fa.flatMap(f)

      override def handleErrorWith[A](fa: Program[F, T, E, A])(f: E => Program[F, T, E, A]): Program[F, T, E, A] =
        fa match {
          case Error(e, _) => f(e)
          case other       => other
        }

      override def raiseError[A](e: E): Program[F, T, E, A] =
        Program.raiseError(e)

      override def pure[A](a: A): Program[F, T, E, A] =
        Program.pure(a)
    }
}
