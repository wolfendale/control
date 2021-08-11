package wolfendale

import cats.data._

package object control {

  type Step[F[_], M, S, E, A] = F[(Program[F, M, S], (Chain[Program[F, M, S]], Either[E, A]))]
  type Machine[F[_], M, S, E, A] = EitherT[WriterT[StateT[F, Program[F, M, S], *], Chain[Program[F, M, S]], *], E, A]
}
