package wolfendale.control.machine

import cats._
import cats.implicits._
import io.iteratee.Iteratee
import wolfendale.control.Program

final case class Trace[F[_], T, E, A](sink: Trace.Sink[F, T, E, A]) extends Machine[F, T, E, A, Vector[Program.Step[F, T, E, A]]] {

  def step(implicit M: Monad[F]): Trace[F, T, E, A] =
    andThen(Iteratee.take(1))

  def whilst(f: T => Boolean)(implicit M: Monad[F]): Trace[F, T, E, A] = {
    val takeWhile = Iteratee.takeWhile[F, Program.Step[F, T, E, A]] {
      case Program.Step.Continue(p) if p.annotation.forall(f) => true
      case _                                                  => false
    }
    andThen(takeWhile)
  }

  def until(f: T => Boolean)(implicit M: Monad[F]): Trace[F, T, E, A] = {
    val takeUntil = Iteratee.takeWhile[F, Program.Step[F, T, E, A]] {
      case Program.Step.Continue(p) if p.annotation.exists(f) => false
      case _                                                  => true
    }
    andThen(takeUntil)
  }

  def to(t: T)(implicit M: Monad[F]): Trace[F, T, E, A] =
    until(_ == t)

  def toCompletion(implicit M: Monad[F]): Trace[F, T, E, A] =
    andThen(Iteratee.consume)

  def toNextAnnotation(implicit M: Monad[F]): Trace[F, T, E, A] = {
    val continue = Iteratee.takeWhile[F, Program.Step[F, T, E, A]] {
      case Program.Step.Continue(p) if p.annotation.isDefined => false
      case _                                                  => true
    }
    andThen(continue)
  }

  def andThen(next: Trace.Sink[F, T, E, A])(implicit M: Monad[F]): Trace[F, T, E, A] =
    Trace(sink |+| next)
}

object Trace {

  type Sink[F[_], T, E, A] = Iteratee[F, Program.Step[F, T, E, A], Vector[Program.Step[F, T, E, A]]]

  def empty[F[_], T, E, A](implicit A: Applicative[F]): Trace[F, T, E, A] =
    Trace(Iteratee.identity[F, Program.Step[F, T, E, A]].as(Vector.empty))
}