package wolfendale.control

import cats.MonadError
import wolfendale.control.Program.Suspend

import scala.language.implicitConversions

package object syntax {

  implicit class Programmable[F[_], A](fa: F[A]) {

    def blank[T, E](implicit ME: MonadError[F, E]): Program[F, T, E, A] =
      Suspend(fa)

    def annotated[T, E](annotation: T)(implicit ME: MonadError[F, E]): Program[F, T, E, A] =
      Suspend(fa, Some(annotation))

    def @@[T, E](annotation: T)(implicit ME: MonadError[F, E]): Program[F, T, E, A] =
      annotated(annotation)
  }

  object @@ {

    def unapply[F[_], T, E, A](pfa: Program[F, T, E, A]): Option[(Program[F, T, E, A], T)] =
      pfa.annotation.map(p => (pfa, p))
  }
}
