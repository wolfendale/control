package wolfendale.control.machine

import cats._
import wolfendale.control.Program

final case class Builder[T](ops: Vector[Builder.Op[T]]) {

  import Builder._

  def step: Builder[T] = andThen(Op.Step())
  def whilst(f: T => Boolean): Builder[T] = andThen(Op.Whilst(f))
  def until(f: T => Boolean): Builder[T] = andThen(Op.Until(f))
  def to(annotation: T): Builder[T] = andThen(Op.To(annotation))
  def toCompletion: Builder[T] = andThen(Op.ToCompletion())
  def toNextAnnotation: Builder[T] = andThen(Op.ToNextAnnotation())

  def trace[F[_], E, A](implicit M: Monad[F]): Trace[F, T, E, A] =
    ops.foldLeft(Trace.empty[F, T, E, A]) { (m, n) =>
      n match {
        case Op.Step()             => m.step
        case Op.Whilst(predicate)  => m.whilst(predicate)
        case Op.Until(predicate)   => m.until(predicate)
        case Op.To(annotation)     => m.to(annotation)
        case Op.ToCompletion()     => m.toCompletion
        case Op.ToNextAnnotation() => m.toNextAnnotation
      }
    }

  def runTrace[F[_], E, A](program: Program[F, T, E, A])(implicit M: MonadError[F, E]): F[Vector[Program.Step[F, T, E, A]]] =
    trace[F, E, A].run(program)

  private def andThen(op: Op[T]): Builder[T] = Builder(ops :+ op)
}

object Builder {

  sealed abstract class Op[T]

  object Op {
    final case class Step[T]() extends Op[T]
    final case class Whilst[T](f: T => Boolean) extends Op[T]
    final case class Until[T](f: T => Boolean) extends Op[T]
    final case class To[T](annotation: T) extends Op[T]
    final case class ToCompletion[T]() extends Op[T]
    final case class ToNextAnnotation[T]() extends Op[T]
  }

  def annotatedWith[T]: Builder[T] = Builder(Vector.empty)

  def complete[T]: Builder[T] =
    annotatedWith[T].toCompletion
}
