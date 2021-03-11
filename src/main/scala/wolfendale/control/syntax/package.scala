package wolfendale.control

import cats.MonadError
import wolfendale.control.Program.Suspend

import scala.language.implicitConversions

package object syntax {

  implicit class Programmable[F[_], A](fa: F[A]) {

    def @@[T, T0 >: T, E](annotation: T)(implicit ME: MonadError[F, E]): Program[F, T0, E, A] =
      @@(Some(annotation))

    def @@[T, T0 >: T, E](annotation: Option[T])(implicit ME: MonadError[F, E]): Program[F, T0, E, A] =
      Suspend(fa, annotation)
  }
}
