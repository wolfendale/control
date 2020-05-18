package wolfendale.control

import scala.annotation.implicitNotFound

@implicitNotFound("${S} does not contain type ${A}")
abstract class Coproduct[S, A] {
  def apply(a: A): S
  def unapply(s: S): Option[A]
}

object Coproduct {

  implicit def identityInstance[A]: Coproduct[A, A] =
    new Coproduct[A, A] {
      override def apply(a: A): A = a
      override def unapply(a: A): Option[A] = Some(a)
    }

  implicit def leftInstance[A, B]: Coproduct[Either[A, B], A] =
    new Coproduct[Either[A, B], A] {
      override def apply(a: A): Either[A, B] = Left(a)
      override def unapply(s: Either[A, B]): Option[A] = s.swap.toOption
    }

  implicit def nestedLeftInstance[A, B, C](implicit ev: Coproduct[A, C]): Coproduct[Either[A, B], C] =
    new Coproduct[Either[A, B], C] {
      override def apply(c: C): Either[A, B] = Left(ev(c))
      override def unapply(s: Either[A, B]): Option[C] = s.swap.toOption.flatMap(ev.unapply)
    }

  implicit def rightInstance[A, B]: Coproduct[Either[A, B], B] =
    new Coproduct[Either[A, B], B] {
      override def apply(b: B): Either[A, B] = Right(b)
      override def unapply(s: Either[A, B]): Option[B] = s.toOption
    }

  implicit def nestedRightInstance[A, B, C](implicit ev: Coproduct[B, C]): Coproduct[Either[A, B], C] =
    new Coproduct[Either[A, B], C] {
      override def apply(c: C): Either[A, B] = Right(ev(c))
      override def unapply(s: Either[A, B]): Option[C] = s.toOption.flatMap(ev.unapply)
    }
}

object CoproductTest extends App {

  val C = implicitly[Coproduct[Either[String, Either[Long, Int]], Int]]

  val x = C(1)
  val C(a) = x

  println(x)
  println(a)
}