package wolfendale.control

import wolfendale.control.Program.Suspend

import scala.language.implicitConversions

package object syntax {

  implicit class Programmable[F[_], A](fa: F[A]) {

    def blank[T]: Program[F, T, A] =
      Suspend(fa)

    def @@[T]: Program[F, T, A] =
      blank

    def annotated[T](annotation: T): Program[F, T, A] =
      Suspend(fa, Some(annotation))

    def @@[T](annotation: T): Program[F, T, A] =
      annotated(annotation)
  }

  object @@ {

    def unapply[F[_], T, A](pfa: Program[F, T, A]): Option[(Program[F, T, A], T)] =
      pfa.annotation.map(p => (pfa, p))
  }
}
