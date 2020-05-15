package wolfendale.control

import cats._
import cats.data._
import cats.syntax.all._
import io.iteratee.Enumerator

import scala.language.{existentials, higherKinds, implicitConversions}

sealed abstract class Program[F[_], T, A] {

  import Program._

  def annotation: Option[T]

  @inline
  final def flatMap[B](f: A => Program[F, T, B]): Program[F, T, B] =
    this match {
      case c: Continue[F, T,  A] => Continue(c.nested, (Kleisli(c.f) >>> Kleisli(f)).run, c.annotation)
      case r: Result[F, T, A]    => Continue(r, f, r.annotation)
    }

  @inline
  final def map[B](f: A => B): Program[F, T, B] =
    flatMap(a => Pure(f(a)))

  final def enumerate(implicit M: Monad[F]): Enumerator[F, Either[Program[F, T, A], A]] =
    eval.enumerator(this)
}

object Program {

  sealed abstract class Result[F[_], T, A] extends Program[F, T, A]

  final case class Pure[F[_], T, A](a: A, annotation: Option[T] = None) extends Result[F, T, A] {

    override def toString: String =
      s"Pure($a)${annotation.map(a => s" @@ $a").mkString}"
  }

  final case class Suspend[F[_], T, A](fa: F[A], annotation: Option[T] = None) extends Result[F, T, A] {

    override def toString: String =
      s"Suspend($fa)${annotation.map(a => s" @@ $a").mkString}"
  }

  abstract class Continue[F[_], T, B] extends Program[F, T, B] {

    type X
    val nested: Result[F, T, X]
    val f: X => Program[F, T, B]

    @inline
    final def step(implicit A: Applicative[F]): F[Program[F, T, B]] = nested match {
      case Pure(value, _)    => A.pure(f(value))
      case Suspend(value, _) => value.map(f)
    }

    override def toString: String =
      s"Continue($nested, <function>)${annotation.map(a => s" @@ $a").mkString}"
  }

  object Continue {

    type Aux[F[_], T, A, B] = Continue[F, T, B] { type X = A }

    def apply[F[_], T, A, B](pfa: Result[F, T, A], g: A => Program[F, T, B], a: Option[T] = None): Aux[F, T, A, B] =
      new Continue[F, T, B] {
        override type X = A
        override val nested: Result[F, T, A] = pfa
        override val f: A => Program[F, T, B] = g
        override val annotation: Option[T] = a // orElse current.annotation // TODO not sure if this should happen
      }
  }

  def pure[F[_], T, A](a: A): Program[F, T, A] =
    Pure(a)

  def suspend[F[_], T, A](fa: F[A]): Program[F, T, A] =
    Suspend(fa)

  implicit def monadInstance[F[_], T]: Monad[Program[F, T, *]] =
    new Monad[Program[F, T, *]] with StackSafeMonad[Program[F, T, *]] {

      override def pure[A](a: A): Program[F, T, A] =
        Program.pure(a)

      override def flatMap[A, B](fa: Program[F, T, A])(f: A => Program[F, T, B]): Program[F, T, B] =
        fa.flatMap(f)
    }
}
