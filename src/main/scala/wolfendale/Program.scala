package wolfendale

import cats._
import cats.implicits._

import scala.annotation.tailrec
import scala.language.{existentials, higherKinds}

sealed abstract class Program[F[_], A] {

  import Program._

  @inline
  final def flatMap[B](f: A => Program[F, B]): Program[F, B] =
    Continue(this, f)

  @inline
  final def map[B](f: A => B): Program[F, B] =
    flatMap(a => Pure(f(a)))

  final def intercept(f: PartialFunction[Program[F, A], Program[F, A]]): Program[F, A] =
    f.lift(this).getOrElse(this)

  @tailrec
  final def step(implicit A: Applicative[F]): ResultT[F, A] =
    this match {
      case Pure(a)           => A.pure(Right(a))
      case Suspend(fa)       => fa.map(Right(_))
      case c: Continue[F, A] => c.current match {
        case Pure(a)              => A.pure(Left(c.sub(a)))
        case Suspend(fx)          => fx.map(cx => Left(c.sub(cx)))
        case c2: Continue[F, c.X] =>
          c2.current.flatMap(c2x => c2.sub(c2x).flatMap(c.sub)).step
      }
    }
}

object Program {

  type ResultT[F[_], A] = F[Either[Program[F, A], A]]

  final case class Pure[F[_], A](a: A) extends Program[F, A]
  final case class Suspend[F[_], A](fa: F[A]) extends Program[F, A]

  abstract class Continue[F[_], B] extends Program[F, B] {

    type X
    val current: Program[F, X]
    val sub: X => Program[F, B]

    override def toString: String =
      s"Continue($current, ...)"
  }

  object Continue {

    type Aux[F[_], A, B] = Continue[F, B] { type X = A }

    def apply[F[_], A, B](pfa: Program[F, A], f: A => Program[F, B]): Aux[F, A, B] =
      new Continue[F, B] {
        override type X = A
        override val current: Program[F, A] = pfa
        override val sub: A => Program[F, B] = f
      }
  }

  def pure[F[_], A](a: A): Program[F, A] =
    Pure(a)

  def suspend[F[_], A](fa: F[A]): Program[F, A] =
    Suspend(fa)

  implicit def monadInstance[F[_]]: Monad[Program[F, *]] =
    new Monad[Program[F, *]] with StackSafeMonad[Program[F, *]] {

      override def pure[A](a: A): Program[F, A] =
        Program.pure(a)

      override def flatMap[A, B](fa: Program[F, A])(f: A => Program[F, B]): Program[F, B] =
        fa.flatMap(f)
    }

//  implicit def monadError[F[_], E](implicit A: ApplicativeError[F, E]): MonadError[Program[F, *], E] =
//    new ApplicativeError[Program[F, *], E] {
//
//      override def raiseError[A](e: E): Program[F, A] =
//        Suspend(A.raiseError(e))
//
//      override def handleErrorWith[A](pfa: Program[F, A])(f: E => Program[F, A]): Program[F, A] =
//        pfa match {
//          case Suspend(fa)     => A.handleErrorWith(fa)(f)
//          case programNoErrors => programNoErrors
//        }
//    }

  object Syntax {

    implicit class Programmable[F[_], A](fa: F[A]) {

      def blank: Program[F, A] =
        Suspend(fa)
    }
  }
}
