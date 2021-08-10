package wolfendale.control

import wolfendale.control.Program.Suspend

import scala.language.implicitConversions

object syntax {

  implicit class ToProgram[F[_], A](fa: F[A]) {

    def annotated[M](m: M): Program[F, M, A] =
      Suspend(fa, Some(m))

    def toProgram[M]: Program[F, M, A] =
      Suspend(fa, None)
  }

  def annotatedWith[M](meta: M): PartiallyAppliedAnnotatedWith[M] =
    new PartiallyAppliedAnnotatedWith[M](meta)

  final class PartiallyAppliedAnnotatedWith[M](meta: M) {
    def apply[F[_], A](fa: F[A]): Program[F, M, A] = Suspend(fa, Some(meta))
  }

  def toProgram[M]: PartiallyAppliedToProgram[M] =
    new PartiallyAppliedToProgram[M]

  final class PartiallyAppliedToProgram[M](private val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], A](fa: F[A]): Program[F, M, A] = Suspend(fa, None)
  }
}
