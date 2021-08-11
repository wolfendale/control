package wolfendale.control

import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._
import wolfendale.control.Program.Pure

object Machine {

  def apply[F[_], M, S, E, A](f: Program[F, M, S] => F[(Program[F, M, S], (Chain[Program[F, M, S]], Either[E, A]))])
                             (implicit ev: Applicative[F]): Machine[F, M, S, E, A] = {

    EitherT(WriterT(StateT(f)))
  }

  def apply[F[_], M, S, E, A](
                               onComplete: (S, Program[F, M, S]) => F[A],
                               onContinue: Program[F, M, S] => F[Option[A]],
                               onError: PartialFunction[(E, Program[F, M, S]), F[A]] = PartialFunction.empty
                             )(implicit ev: MonadError[F, E]): Machine[F, M, S, E, Option[A]] = {

    apply {
      case p @ Pure(a, _) =>
        onComplete(a, p).map(b => (p: Program[F, M, S], (Chain.one[Program[F, M, S]](p), b.some.asRight[E])))
      case p1: Program[F, M, S] =>
        onContinue(p1).flatMap { ob =>
          ob.fold {
            p1.step.map { p2 =>
              (p2, (Chain.one[Program[F, M, S]](p1), Option.empty[A].asRight[E]))
            }
          } { b =>
            ev.pure((p1: Program[F, M, S], (Chain.empty[Program[F, M, S]], b.some.asRight[E])))
          }
        }.handleErrorWith { e =>
          onError.lift((e, p1)).fold {
            ev.pure((p1, (Chain.empty[Program[F, M, S]], e.asLeft[Option[A]])))
          } {
            _.map { b =>
              (p1, (Chain.empty, b.some.asRight))
            }
          }
        }
    }
  }

  def step[F[_], M, E, A](implicit ev: MonadError[F, E]): Machine[F, M, A, E, Option[A]] =
    Machine(
      onComplete = (a, _) => ev.pure(a),
      onContinue = _      => ev.pure(None)
    )

  def run[F[_], M, E, A](implicit ev: MonadError[F, E]): Machine[F, M, A, E, A] =
    step[F, M, E, A].untilDefinedM

  def runWhileM[F[_], M, E, A](f: Program[F, M, A] => F[Boolean])(implicit ev: MonadError[F, E]): Machine[F, M, A, E, Program[F, M, A]] =
    Machine[F, M, A, E, Program[F, M, A]](
      onComplete = (_: A, p: Program[F, M, A]) => ev.pure(p),
      onContinue = (p: Program[F, M, A])       => f(p).map(b => if (b) None else p.some)
    ).untilDefinedM

  def runUntilM[F[_], M, E, A](f: Program[F, M, A] => F[Boolean])(implicit ev: MonadError[F, E]): Machine[F, M, A, E, Program[F, M, A]] =
    runWhileM(f(_).map(!_))

  def runWhile[F[_], M, E, A](f: Program[F, M, A] => Boolean)(implicit ev: MonadError[F, E]): Machine[F, M, A, E, Program[F, M, A]] =
    runWhileM(a => ev.pure(f(a)))

  def runWhile[F[_], M, E, A](m: M)(implicit ev: MonadError[F, E]): Machine[F, M, A, E, Program[F, M, A]] =
    runWhile((p: Program[F, M, A]) => p.meta.contains(m))

  def runUntil[F[_], M, E, A](f: Program[F, M, A] => Boolean)(implicit ev: MonadError[F, E]): Machine[F, M, A, E, Program[F, M, A]] =
    runWhile(!f(_))

  def runUntil[F[_], M, E, A](m: M)(implicit ev: MonadError[F, E]): Machine[F, M, A, E, Program[F, M, A]] =
    runUntil((p: Program[F, M, A]) => p.meta.contains(m))

  def resume[F[_], M, E, A](implicit ev: MonadError[F, E]): Machine[F, M, A, E, Program[F, M, A]] =
    Machine[F, M, A, E, Program[F, M, A]](
      onComplete = (_: A, p: Program[F, M, A]) => ev.pure(p),
      onContinue = (p: Program[F, M, A])       => ev.pure(if (p.meta.nonEmpty) p.some else None)
    ).untilDefinedM
}
