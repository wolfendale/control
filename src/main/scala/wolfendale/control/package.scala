package wolfendale

import cats._
import cats.implicits._
import cats.data._

import scala.concurrent.Future

package object control {

  type Machine[F[_], M, S, E, A] = EitherT[WriterT[StateT[F, Program[F, M, S], *], Chain[Program[F, M, S]], *], E, A]
}
