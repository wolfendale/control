package wolfendale

import cats._
import cats.data._
import cats.implicits._

final case class Iter[A](extract: A, rest: Eval[Iter[A]]) {

  // convenience method for unwrapping the next iterator
  def next: Iter[A] = rest.value

  def extend[B](f: Iter[A] => B): Iter[B] =
    Iter(f(this), Eval.later(next.extend(f)))

  def map[B](f: A => B): Iter[B] =
    Iter(f(extract), Eval.later(next.map(f)))

  def :::(nextA: A): Iter[A] = Iter(nextA, Eval.now(this))

  override def toString: String = s"Iter($extract, ...)"
}

object ::: {

  def unapply[A](iter: Iter[A]): Option[(A, Eval[Iter[A]])] =
    Some((iter.extract, iter.rest))

  def unapply[A](iterE: Eval[Iter[A]]): Option[(A, Eval[Iter[A]])] = {
    val iter = iterE.value
    Some((iter.extract, iter.rest))
  }
}

object Iter {

  def continually[A](a: A): Iter[A] = Iter(a, Eval.later(continually(a)))

  def fromList[A](list: List[A]): Iter[Option[A]] =
    list.foldRight(continually(Option.empty[A])) { (n, m) =>
      Some(n) ::: m
    }

  def fromListM[A](list: List[A])(implicit M: Monoid[A]): Iter[A] =
    list.foldRight(continually(M.empty))(_ ::: _)


  implicit val comonad: Comonad[Iter] = new Comonad[Iter] {

    override def extract[A](fa: Iter[A]): A =
      fa.extract

    override def coflatMap[A, B](fa: Iter[A])(f: Iter[A] => B): Iter[B] =
      fa.extend(f)

    override def map[A, B](fa: Iter[A])(f: A => B): Iter[B] =
      fa.map(f)
  }
}
