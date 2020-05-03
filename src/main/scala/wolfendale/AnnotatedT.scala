package wolfendale

import cats._
import cats.data._
import cats.free.{Free, FreeT}
import cats.implicits._

import scala.language.implicitConversions

final case class AnnotatedT[F[_], T, A](value: F[A], annotation: Option[T])

object AnnotatedT {

  type Program[F[_], T, A] = Free[AnnotatedT[F, T, *], A]
  type Annotated[T, A] = AnnotatedT[Id, T, A]

  object Syntax {

    implicit class Annotatable[F[_], A](fa: F[A]) {
      def annotate[T](annotation: T): Program[F, T, A] =
        Free.liftF(AnnotatedT(fa, Some(annotation)))
    }

    implicit def blankAnnotation[F[_], T, A](fa: F[A]): Program[F, T, A] =
      Free.liftF(AnnotatedT(fa, None))
  }

  implicit def functorInstance[F[_], T](implicit F: Functor[F]): Functor[AnnotatedT[F, T, *]] =

    new Functor[AnnotatedT[F, T, *]] {
      override def map[A, B](fa: AnnotatedT[F, T, A])(f: A => B): AnnotatedT[F, T, B] =
        AnnotatedT(fa.value.map(f), None)
    }

  implicit def applicativeInstance[F[_], T](implicit A: Applicative[F]): Applicative[AnnotatedT[F, T, *]] =
    new Applicative[AnnotatedT[F, T, *]] {

      override def pure[A](a: A): AnnotatedT[F, T, A] =
        AnnotatedT(A.pure(a), None)

      override def ap[A, B](ff: AnnotatedT[F, T, A => B])(fa: AnnotatedT[F, T, A]): AnnotatedT[F, T, B] =
        AnnotatedT(A.ap(ff.value)(fa.value), None)
    }

  implicit def monadInstance[F[_], T](implicit M: Monad[F]): Monad[AnnotatedT[F, T, *]] =
    new Monad[AnnotatedT[F, T, *]] {

      override def pure[A](a: A): AnnotatedT[F, T, A] =
        AnnotatedT(M.pure(a), None)

      override def flatMap[A, B](fa: AnnotatedT[F, T, A])(f: A => AnnotatedT[F, T, B]): AnnotatedT[F, T, B] =
        AnnotatedT(fa.value.flatMap(a => f(a).value), None)

      override def tailRecM[A, B](a: A)(f: A => AnnotatedT[F, T, Either[A, B]]): AnnotatedT[F, T, B] =
        AnnotatedT(M.tailRecM(a)(a => f(a).value), None)
    }
}