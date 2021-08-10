package wolfendale.control

import cats.{Monad, MonadError}
import cats.implicits._
import cats.data.{Chain, NonEmptyChain, StateT}

class Builder[F[_]: Monad, M, A] extends Serializable {

  final def step: Machine[F, M, A, Program[F, M, A]] =
    StateT[F, Program[F, M, A], Program[F, M, A]](_.step.map(a => (a, a)))

  final def attemptStep[E](implicit ev: MonadError[F, E]): Machine[F, M, A, Option[E]] =
    StateT[F, Program[F, M, A], Option[E]](_.attemptStep)

  final def run: Machine[F, M, A, A] =
    StateT.inspectF(_.run)

  final def runWhile(f: M => Boolean): Machine[F, M, A, Unit] =
    StateT.modifyF[F, Program[F, M, A]](_.runWhile(f))

  final def runUntil(f: M => Boolean): Machine[F, M, A, Unit] =
    StateT.modifyF[F, Program[F, M, A]](_.runUntil(f))

  final def collect: Machine[F, M, A, NonEmptyChain[Program[F, M, A]]] =
    StateT[F, Program[F, M, A], NonEmptyChain[Program[F, M, A]]](_.collect)

  final def collectWhile(f: M => Boolean): Machine[F, M, A, Chain[Program[F, M, A]]] =
    StateT[F, Program[F, M, A], Chain[Program[F, M, A]]](_.collectWhile(f))

  final def collectUntil(f: M => Boolean): Machine[F, M, A, Chain[Program[F, M, A]]] =
    StateT[F, Program[F, M, A], Chain[Program[F, M, A]]](_.collectUntil(f))

  final def collectTo(f: M => Boolean): Machine[F, M, A, Chain[Program[F, M, A]]] = {
    for {
      steps <- collectUntil(f)
      next  <- step
    } yield if (steps.lastOption.contains(next)) steps else steps :+ next
  }

  final def attempt[E](implicit ev: MonadError[F, E]): Machine[F, M, A, Either[E, A]] =
    StateT[F, Program[F, M, A], Either[E, A]](_.attempt[E])

  final def attemptWhile[E](f: M => Boolean)(implicit ev: MonadError[F, E]): Machine[F, M, A, Option[E]] =
    StateT[F, Program[F, M, A], Option[E]](_.attemptWhile[E](f))

  final def attemptUntil[E](f: M => Boolean)(implicit ev: MonadError[F, E]): Machine[F, M, A, Option[E]] =
    StateT[F, Program[F, M, A], Option[E]](_.attemptUntil[E](f))

  final def attemptCollect[E](implicit ev: MonadError[F, E]): Machine[F, M, A, (NonEmptyChain[Program[F, M, A]], Option[E])] =
    StateT[F, Program[F, M, A], (NonEmptyChain[Program[F, M, A]], Option[E])](_.attemptCollect[E])

  final def attemptCollectWhile[E](f: M => Boolean)(implicit ev: MonadError[F, E]): Machine[F, M, A, (Chain[Program[F, M, A]], Option[E])] =
    StateT[F, Program[F, M, A], (Chain[Program[F, M, A]], Option[E])](_.attemptCollectWhile(f))

  final def attemptCollectUntil[E](f: M => Boolean)(implicit ev: MonadError[F, E]): Machine[F, M, A, (Chain[Program[F, M, A]], Option[E])] =
    StateT[F, Program[F, M, A], (Chain[Program[F, M, A]], Option[E])](_.attemptCollectUntil(f))
}
