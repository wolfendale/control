package wolfendale.control

import cats.Monad

object Machine {

  def apply[F[_], M, A]: MachinePartiallyAppliedApply[F, M, A] =
    new MachinePartiallyAppliedApply[F, M, A]

  final class MachinePartiallyAppliedApply[F[_], M, A] {
    def apply[B](f: Builder[F, M, A] => Machine[F, M, A, B])(implicit ev: Monad[F]): Machine[F, M, A, B] =
      f(new Builder[F, M, A])
  }
}
