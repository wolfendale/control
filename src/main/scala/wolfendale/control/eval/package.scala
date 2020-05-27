package wolfendale.control

import cats._
import cats.implicits._
import io.iteratee.{Enumerator, Iteratee}
import wolfendale.control.Program._

package object eval {

  type ProgramStep[F[_], T, E, A] = Either[Program[F, T, E, A], Either[Short[F, T, E, A], A]]

  def enumerator[F[_], T, E, A](program: Program[F, T, E, A])(implicit M: MonadError[F, E]): Enumerator[F, ProgramStep[F, T, E, A]] = {

    def step(next: ProgramStep[F, T, E, A]): F[Option[ProgramStep[F, T, E, A]]] =
      next match {
        case Right(_)  => M.pure(None)
        case Left(pfa) => pfa.step.map(Some(_))
      }

    Enumerator.iterateUntilM[F, ProgramStep[F, T, E, A]](Left(program))(step)
  }

  implicit def iterateeSemigroup[F[_], E, A](implicit SA: Semigroup[A], MA: Monad[F]): Semigroup[Iteratee[F, E, A]] =
    (x: Iteratee[F, E, A], y: Iteratee[F, E, A]) => for {
      xa <- x
      ya <- y
    } yield xa combine ya
}
