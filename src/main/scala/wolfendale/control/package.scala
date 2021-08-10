package wolfendale

import cats.data.StateT

package object control {

  type Machine[F[_], M, A, B] = StateT[F, Program[F, M, A], B]
}
