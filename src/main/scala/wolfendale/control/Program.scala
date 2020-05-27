package wolfendale.control

import cats._
import cats.data._
import cats.implicits._
import wolfendale.control.eval.ProgramStep

import scala.language.{existentials, higherKinds, implicitConversions}

sealed abstract class Program[F[_], T, E, A] extends Serializable {

  import Program._

  def annotation: Option[T]

  @inline
  final def flatMap[B](f: A => Program[F, T, E, B]): Program[F, T, E, B] =
    this match {
      case s: Short[F, T, E, A]    => s.shiftA[B]
      case c: Continue[F, T, E, A] => Continue(c.nested, (Kleisli(c.f) >>> Kleisli(f)).run, c.annotation)
      case r: Result[F, T, E, A]   => Continue(r, f, r.annotation)
    }

  @inline
  final def map[B](f: A => B): Program[F, T, E, B] =
    flatMap(a => Pure(f(a)))

  final def foldLeft[B](b: B)(f: (B, A) => B)(implicit F: Functor[F], FF: Foldable[F]): B =
    this match {
      case _: Short[F, T, E, A]    => b
      case Pure(a, _)              => f(b, a)
      case Suspend(fa, _)          => fa.foldLeft(b)(f)
      case c: Continue[F, T, E, A] => c.nested match {
        case _: Short[F, T, E, c.X] => b
        case Pure(cx, _)            => c.f(cx).foldLeft(b)(f)
        case Suspend(fcx, _)        => fcx.foldLeft(b)((bb, cx) => c.f(cx).foldLeft(bb)(f))
      }
    }

  final def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Functor[F], FF: Foldable[F]): Eval[B] =
    this match {
      case _: Short[F, T, E, A]    => lb
      case Pure(a, _)              => f(a, lb)
      case Suspend(fa, _)          => fa.foldRight(lb)(f)
      case c: Continue[F, T, E, A] => c.nested match {
        case _: Short[F, T, E, c.X] => lb
        case Pure(cx, _)            => c.f(cx).foldRight(lb)(f)
        case Suspend(fcx, _)        => fcx.foldRight(lb)((cx, bb) => c.f(cx).foldRight(bb)(f))
      }
    }

  final def traverse[G[_], B](f: A => G[B])(implicit F: Functor[F], AG: Applicative[G], TF: Traverse[F]): G[Program[F, T, E, B]] =
    this match {
      case s: Short[F, T, E, A]    => AG.pure(s.shiftA[B])
      case Pure(a, t)              => f(a).map(Pure(_, t))
      case Suspend(fa, t)          => fa.traverse(f).map(Suspend(_, t))
      case c: Continue[F, T, E, A] => c.nested match {
        case s: Short[F, T, E, c.X] => AG.pure(s.shiftA[B])
        case Pure(cx, _)            => c.f(cx).traverse(f)
        case Suspend(fcx, _)        => fcx.traverse(cx => c.f(cx).traverse(f)).map(roll)
      }
    }

  final def step(implicit A: ApplicativeError[F, E]): F[ProgramStep[F, T, E, A]] =
    this match {
      case s: Short[F, T, E, A]    => A.pure(Right(Left(s)))
      case Pure(a, _)              => A.pure(Right(Right(a)))
      case Suspend(fa, _)          => fa.map(a => Right(Right(a)))
      case c: Continue[F, T, E, A] => c.continue.redeem(e => Right(Left(Error(e, c.annotation))), Left(_))
    }
}

object Program {

  sealed abstract class Result[F[_], T, E, A] extends Program[F, T, E, A] with Serializable

  final case class Pure[F[_], T, E, A](a: A, annotation: Option[T] = None) extends Result[F, T, E, A] {
    override def toString: String =
      s"pure${annotation.map(a => s" @@ $a").mkString}"
  }

  final case class Suspend[F[_], T, E, A](fa: F[A], annotation: Option[T] = None) extends Result[F, T, E, A] {
    override def toString: String =
      s"suspend${annotation.map(a => s" @@ $a").mkString}"
  }

  sealed abstract class Short[F[_], T, E, A] extends Result[F, T, E, A] with Serializable {

    @inline
    final def shiftA[B]: Short[F, T, E, B] =
      this match {
        case Halt(t)     => Halt(t)
        case Error(e, t) => Error(e, t)
      }
  }

  final case class Halt[F[_], T, E, A](annotation: Option[T]) extends Short[F, T, E, A]
  final case class Error[F[_], T, E, A](error: E, annotation: Option[T] = None) extends Short[F, T, E, A]

  abstract class Continue[F[_], T, E, B] extends Program[F, T, E, B] with Serializable {

    type X
    val nested: Result[F, T, E, X]
    val f: X => Program[F, T, E, B]

    @inline
    final def continue(implicit A: Applicative[F]): F[Program[F, T, E, B]] = nested match {
      case s: Short[F, T, E, X] => A.pure(s.shiftA[B])
      case Pure(a, _)           => A.pure(f(a))
      case Suspend(fa, _)       => fa.map(f)
    }

    override def toString: String =
      s"continue${annotation.map(a => s" @@ $a").mkString}"
  }

  object Continue {

    type Aux[F[_], T, E, A, B] = Continue[F, T, E, B] { type X = A }

    def apply[F[_], T, E, A, B](pfa: Result[F, T, E, A], g: A => Program[F, T, E, B], a: Option[T] = None): Aux[F, T, E, A, B] =
      new Continue[F, T, E, B] {
        override type X = A
        override val nested: Result[F, T, E, A] = pfa
        override val f: A => Program[F, T, E, B] = g
        override val annotation: Option[T] = a
      }
  }

  @inline
  def pure[F[_], T, E, A](a: A): Program[F, T, E, A] =
    Pure(a)

  @inline
  def suspend[F[_], T, E, A](fa: F[A]): Program[F, T, E, A] =
    Suspend(fa)

  @inline def raiseError[F[_], T, E, A](e: E): Program[F, T, E, A] =
    Error(e)

  @inline
  def roll[F[_], T, E, A](fp: F[Program[F, T, E, A]]): Program[F, T, E, A] =
    suspend(fp).flatMap(identity) // Continue(Suspend, identity)

  implicit def monadInstance[F[_], T, E]: Monad[Program[F, T, E, *]] =
    new Monad[Program[F, T, E, *]] with StackSafeMonad[Program[F, T, E, *]] {

      override def pure[A](a: A): Program[F, T, E, A] =
        Program.pure(a)

      override def flatMap[A, B](fa: Program[F, T, E, A])(f: A => Program[F, T, E, B]): Program[F, T, E, B] =
        fa.flatMap(f)

      override def map[A, B](fa: Program[F, T, E, A])(f: A => B): Program[F, T, E, B] =
        fa.map(f)
    }

  implicit def traverseInstance[F[_], T, E](implicit TF: Traverse[F], FF: Foldable[F]): Traverse[Program[F, T, E, *]] =
    new Traverse[Program[F, T, E, *]] {

      override def traverse[G[_], A, B](fa: Program[F, T, E, A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[Program[F, T, E, B]] =
        fa.traverse(f)

      override def foldLeft[A, B](fa: Program[F, T, E, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: Program[F, T, E, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)
    }

  implicit def monadErrorInstance[F[_], T, E](implicit E: MonadError[F, E]): MonadError[Program[F, T, E, *], E] =
    new MonadError[Program[F, T, E, *], E] with StackSafeMonad[Program[F, T, E, *]] {

      override def flatMap[A, B](fa: Program[F, T, E, A])(f: A => Program[F, T, E, B]): Program[F, T, E, B] =
        fa.flatMap(f)

      override def handleErrorWith[A](fa: Program[F, T, E, A])(f: E => Program[F, T, E, A]): Program[F, T, E, A] =
        fa match {
          case Error(e, _) => f(e)
          case other       => other
        }

      override def raiseError[A](e: E): Program[F, T, E, A] =
        Program.raiseError(e)

      override def pure[A](a: A): Program[F, T, E, A] =
        Program.pure(a)
    }
}
