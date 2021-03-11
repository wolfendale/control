package wolfendale.control

import cats._
import cats.implicits._
import io.iteratee.Iteratee

package object machine {

  implicit def iterateeSemigroup[F[_], E, A](implicit SA: Semigroup[A], MA: Monad[F]): Semigroup[Iteratee[F, E, A]] =
    (x: Iteratee[F, E, A], y: Iteratee[F, E, A]) => for {
      xa <- x
      ya <- y
    } yield xa combine ya
}
