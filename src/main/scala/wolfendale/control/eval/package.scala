package wolfendale.control

import cats._
import cats.implicits._
import io.iteratee.Enumerator
import wolfendale.control.Program._

package object eval {

  def enumerator[F[_], T, A](program: Program[F, T, A])(implicit M: Monad[F]): Enumerator[F, Either[Program[F, T, A], A]] = {

    def step(next: Either[Program[F, T, A], A]): F[Option[Either[Program[F, T, A], A]]] =
      next match {
        case Right(_)  => M.pure(None)
        case Left(pfa) => pfa match {
          case Pure(a, _)           => M.pure(Some(Right(a)))
          case Suspend(fa, _)       => fa.map(a => Some(Left(Program.pure(a))))
          case c: Continue[F, T, A] => c.step.map(next => Some(Left(next)))
        }
      }

    Enumerator.iterateUntilM[F, Either[Program[F, T, A], A]](Left(program))(step)
  }
}
