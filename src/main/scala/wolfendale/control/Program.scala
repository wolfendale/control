package wolfendale.control

import cats._
import cats.data._
import cats.implicits._

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

  final def foldLeft[B](b: B)(f: (B, A) => B)(implicit F: Functor[F], FF: Foldable[F]): B =
    this match {
      case Pure(a, _)           => f(b, a)
      case Suspend(fa, _)       => fa.foldLeft(b)(f)
      case c: Continue[F, T, A] => c.nested match {
        case Pure(cx, _)          => c.f(cx).foldLeft(b)(f)
        case Suspend(fcx, _)      => fcx.foldLeft(b)((bb, cx) => c.f(cx).foldLeft(bb)(f))
      }
    }

  final def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Functor[F], FF: Foldable[F]): Eval[B] =
    this match {
      case Pure(a, _)           => f(a, lb)
      case Suspend(fa, _)       => fa.foldRight(lb)(f)
      case c: Continue[F, T, A] => c.nested match {
        case Pure(cx, _)          => c.f(cx).foldRight(lb)(f)
        case Suspend(fcx, _)      => fcx.foldRight(lb)((cx, bb) => c.f(cx).foldRight(bb)(f))
      }
    }

  final def traverse[G[_], B](f: A => G[B])(implicit F: Functor[F], AG: Applicative[G], TF: Traverse[F]): G[Program[F, T, B]] =
    this match {
      case Pure(a, t)           => f(a).map(Pure(_, t))
      case Suspend(fa, t)       => fa.traverse(f).map(Suspend(_, t))
      case c: Continue[F, T, A] => c.nested match {
        case Pure(cx, _)          => c.f(cx).traverse(f)
        case Suspend(fcx, _)      => fcx.traverse(cx => c.f(cx).traverse(f)).map(roll)
      }
    }

  def step(implicit A: Applicative[F]): F[Either[Program[F, T, A], A]] =
    this match {
      case r: Result[F, T, A]   => r.value.map(Right(_))
      case c: Continue[F, T, A] => c.nested match {
        case Pure(cx, _)          => A.pure(Left(c.f(cx)))
        case Suspend(fcx, _)      => fcx.map(cx => Left(c.f(cx)))
      }
    }
}

object Program {

  sealed abstract class Result[F[_], T, A] extends Program[F, T, A] {
    final def value(implicit A: Applicative[F]): F[A] = this match {
      case Pure(a, _)     => A.pure(a)
      case Suspend(fa, _) => fa
    }
  }

  final case class Pure[F[_], T, A](a: A, annotation: Option[T] = None) extends Result[F, T, A] {
    override def toString: String =
      s"pure${annotation.map(a => s" @@ $a").mkString}"
  }

  final case class Suspend[F[_], T, A](fa: F[A], annotation: Option[T] = None) extends Result[F, T, A] {
    override def toString: String =
      s"suspend${annotation.map(a => s" @@ $a").mkString}"
  }

  abstract class Continue[F[_], T, B] extends Program[F, T, B] {

    type X
    val nested: Result[F, T, X]
    val f: X => Program[F, T, B]

    final def continue(implicit A: Applicative[F]): F[Program[F, T, B]] = nested match {
      case Pure(a, _)     => A.pure(f(a))
      case Suspend(fa, _) => fa.map(f)
    }

    override def toString: String =
      s"continue${annotation.map(a => s" @@ $a").mkString}"
  }

  object Continue {

    type Aux[F[_], T, A, B] = Continue[F, T, B] { type X = A }

    def apply[F[_], T, A, B](pfa: Result[F, T, A], g: A => Program[F, T, B], a: Option[T] = None): Aux[F, T, A, B] =
      new Continue[F, T, B] {
        override type X = A
        override val nested: Result[F, T, A] = pfa
        override val f: A => Program[F, T, B] = g
        override val annotation: Option[T] = a
      }
  }

  @inline
  def pure[F[_], T, A](a: A): Program[F, T, A] =
    Pure(a)

  @inline
  def suspend[F[_], T, A](fa: F[A]): Program[F, T, A] =
    Suspend(fa)

  @inline
  def roll[F[_], T, A](fp: F[Program[F, T, A]]): Program[F, T, A] =
    suspend(fp).flatMap(identity) // Continue(Suspend, identity)

  implicit def monadInstance[F[_], T]: Monad[Program[F, T, *]] =
    new Monad[Program[F, T, *]] with StackSafeMonad[Program[F, T, *]] {

      override def pure[A](a: A): Program[F, T, A] =
        Program.pure(a)

      override def flatMap[A, B](fa: Program[F, T, A])(f: A => Program[F, T, B]): Program[F, T, B] =
        fa.flatMap(f)

      override def map[A, B](fa: Program[F, T, A])(f: A => B): Program[F, T, B] =
        fa.map(f)
    }

  implicit def traverseInstance[F[_], T](implicit TF: Traverse[F], FF: Foldable[F]): Traverse[Program[F, T, *]] =
    new Traverse[Program[F, T, *]] {

      override def traverse[G[_], A, B](fa: Program[F, T, A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[Program[F, T, B]] =
        fa.traverse(f)

      override def foldLeft[A, B](fa: Program[F, T, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: Program[F, T, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)
    }
}
