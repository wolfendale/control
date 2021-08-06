package wolfendale.control.v2

import cats._
import cats.data.NonEmptyChain
import cats.implicits._

import scala.language.implicitConversions

/**
 * This is essentially a `FreeT` monad transformer which also provides meta data for
 * different steps within the program, allowing us to write interpreters which use
 * that meta data to determine how the program should be executed
 *
 * @tparam F the functor / monad that we are wrapping
 * @tparam M the type of the metadata
 * @tparam A the value type
 */
sealed abstract class Program[F[_], M, A] {

  import Program._

  /**
   * This is the metadata associated with a particular step in the program,
   * as certain steps may not have any extra metadata this is optional.
   *
   * @return
   */
  def meta: Option[M]

  /**
   * We implement `flatMap` by building up the free monadic structure,
   * each flatMap nests `this` within a `Continue` with the function (`f`) as
   * the second parameter. Here you can see that we never _apply_ `f` here
   * which shows that we're just building up a program as a data structure
   * rather than evaluating anything.
   *
   * The implementation for `Result` is fairly obvious, however for `Continue`
   * we prevent nesting `Contines` together by using the nested program from
   * the continue and composing its function with our new one, this
   * helps to make sure that we maintain stack safety.
   *
   * This is imlemented in a slightly different way in `cats.free` as they
   * allow for left nested `Continue`s but then at runtime do the remapping
   * this is less efficient if you end up running the same program multiple
   * times.
   *
   * @param f
   * @tparam B
   * @return
   */
  final def flatMap[B](f: A => Program[F, M, B]): Program[F, M, B] = {
    this match {
      case r: Result[F, M, A]   => Continue(r, f, r.meta)
      case c: Continue[F, M, A] => Continue(c.nested, (x: c.X) => c.f(x).flatMap(f), c.meta)
    }
  }

  final def map[B](f: A => B): Program[F, M, B] =
    flatMap(a => Pure(f(a)))

  // TODO test
  final def foldLeft[B](b: B)(f: (B, A) => B)(implicit FF: Foldable[F]): B =
    this match {
      case Pure(a, _)           => f(b, a)
      case Suspend(fa, _)       => fa.foldLeft(b)(f)
      case c: Continue[F, M, A] => c.nested match {
        case Pure(x, _)     => c.f(x).foldLeft(b)(f)
        case Suspend(fx, _) => fx.foldLeft(b)((bb, x) => c.f(x).foldLeft(bb)(f))
      }
    }

  // TODO test
  final def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit FF: Foldable[F]): Eval[B] =
    this match {
      case Pure(a, _)           => f(a, lb)
      case Suspend(fa, _)       => fa.foldRight(lb)(f)
      case c: Continue[F, M, A] => c.nested match {
        case Pure(x, _)     => c.f(x).foldRight(lb)(f)
        case Suspend(fx, _) => fx.foldRight(lb)((x, bb) => c.f(x).foldRight(bb)(f))
      }
    }

  // TODO test
  final def traverse[G[_], B](f: A => G[B])(implicit A: Applicative[G], T: Traverse[F]): G[Program[F, M, B]] =
    this match {
      case Pure(a, _)           => f(a).map(Pure(_))
      case Suspend(fa, _)       => fa.traverse(f).map(Suspend(_))
      case c: Continue[F, M, A] => c.nested match {
        case Pure(x, _)     => c.f(x).traverse(f)
        case Suspend(fx, _) => fx.traverse(x => c.f(x).traverse(f)).map(roll)
      }
    }

  final def isDone: Boolean =
    this match {
      case _: Result[F, M, A]   => true
      case _: Continue[F, M, A] => false
    }

  final def step(implicit ev: Applicative[F]): F[Program[F, M, A]] =
    this match {
      case Pure(a, _)           => ev.pure(Pure(a))
      case c: Continue[F, M, A] => c.continue
    }

  final def runUntil(f: M => Boolean)(implicit ev: Monad[F]): F[Program[F, M, A]] =
    Monad[F].iterateUntilM(this)(_.step)(p => p.meta.exists(f) || p.isDone)

  final def runWhile(f: M => Boolean)(implicit ev: Monad[F]): F[Program[F, M, A]] =
    Monad[F].iterateWhileM(this)(_.step)(p => p.meta.exists(f) || p.isDone)

  final def run(implicit ev: Monad[F]): F[A] = {

    def next(p: Program[F, M, A])(implicit ev: Applicative[F]): F[Either[Program[F, M, A], A]] =
      p match {
        case r: Result[F, M, A]   => r.resolve(Right(_))
        case c: Continue[F, M, A] => c.continue.map(Left(_))
      }

    Monad[F].tailRecM(this)(next)
  }

  final def collectUntil(f: M => Boolean)(implicit ev: Monad[F]): F[NonEmptyChain[Program[F, M, A]]] =
    Monad[F].tailRecM(NonEmptyChain.one(this)) { steps =>
      val current = steps.last
      if (current.meta.exists(f) || current.isDone) ev.pure(Right(steps)) else current.step.map(next => Left(steps :+ next))
    }

  final def collectWhile(f: M => Boolean)(implicit ev: Monad[F]): F[NonEmptyChain[Program[F, M, A]]] =
    collectUntil(!f(_))

  final def collect(implicit ev: Monad[F]): F[NonEmptyChain[Program[F, M, A]]] =
    Monad[F].tailRecM(NonEmptyChain.one(this)) { steps =>
      val current = steps.last
      if (current.isDone) ev.pure(Right(steps)) else current.step.map(next => Left(steps :+ next))
    }
}

object Program {

  /**
   * This is a class we use to combine `Pure` and `Suspend` as they are essentially the same
   * thing structurally.
   *
   * By doing this, we can also constrain `Continue` to only allow nested `Result`s instead of
   * whole nested `Program`s. This is good because we enforce stack safety at compile time
   *
   * @tparam F the functor / monad that we are wrapping
   * @tparam M the type of the metadata
   * @tparam A the value type
   */
  sealed abstract class Result[F[_], M, A] extends Program[F, M, A] {

    @inline
    final def resolve[B](f: A => B)(implicit ev: Applicative[F]): F[B] =
      this match {
        case Pure(a, _)     => ev.pure(f(a))
        case Suspend(fa, _) => fa.map(f)
      }
  }

  /**
   * An entirely pure value lifted into the program.
   *
   * @param a
   * @param meta
   * @tparam F the functor / monad that we are wrapping
   * @tparam M the type of the metadata
   * @tparam A the value type
   */
  final case class Pure[F[_], M, A](a: A, meta: Option[M] = None) extends Result[F, M, A]

  /**
   * A value suspended in our nested `F[_]` functor.
   *
   * @param fa
   * @param meta
   * @tparam F the functor / monad that we are wrapping
   * @tparam M the type of the metadata
   * @tparam A the value type
   */
  final case class Suspend[F[_], M, A](fa: F[A], meta: Option[M] = None) extends Result[F, M, A]

  /**
   * Representation of the `flatMap` method in the free monadic structure
   *
   * The encoding here can be quite confusing,
   *
   * Essentially this wraps an inner `Result` and a function to map its inner value to `A`.
   * (This is a `Result` and not a `Program` to maintain stack safety)
   *
   * However, the inner value of the nested result could be of any other type, and the
   * corresponding function must match its type. This _could_ be encoded as an extra
   * type parameter for this class, however this causes issues when trying to define
   * certain methods.
   *
   * @tparam F the functor / monad that we are wrapping
   * @tparam M the type of the metadata
   * @tparam A the value type
   */
  sealed abstract class Continue[F[_], M, A] extends Program[F, M, A] {

    type X
    val nested: Result[F, M, X]
    val f: X => Program[F, M, A]

    /**
     * This is a useful helper method which returns a new `Program` (suspended in `F`)
     * which is generated by `running` this layer of the free monad.
     *
     * This is especially useful because it means that you can `run` a `Continue`
     * without ever needing to consider its inner `X` type
     *
     * @param ev
     * @return
     */
    final def continue(implicit ev: Applicative[F]): F[Program[F, M, A]] = nested match {
      case Pure(x, _)     => ev.pure(f(x))
      case Suspend(fx, _) => fx.map(f)
    }

    override def toString: String = s"Continue($nested, f(...), $meta)"
  }

  object Continue {

    /**
     * Helper method to create a `Continue` instance
     *
     * @param nested0
     * @param f0
     * @param meta0
     * @tparam F
     * @tparam M
     * @tparam A
     * @tparam X0
     * @return
     */
    def apply[F[_], M, A, X0](nested0: Result[F, M, X0], f0: X0 => Program[F, M, A], meta0: Option[M] = None): Continue[F, M, A] =
      new Continue[F, M, A] {
        override type X = X0
        override val nested: Result[F, M, X0] = nested0
        override val f: X0 => Program[F, M, A] = f0
        override val meta: Option[M] = meta0
      }
  }

  def pure[F[_], M]: PartiallyAppliedPureApply[F, M] = new PartiallyAppliedPureApply[F, M]

  /**
   * As `pure` takes a single input of type `A`, in order to type check we need to supply what
   * `F[_]` and `M` are. As we can only have a single set of type parameters for a function that
   * would usually also require us to include `A` in that list e.g.
   *
   * def pure[F[_], M, A] = ...
   * pure[Eval, String, Int](1)
   *
   * However, as we are passing an `A` we can break the inference into two stages by using this
   * `PartiallyAppliedPureApply` class, allowing us to do the following:
   *
   * pure[Eval, String](1)
   *
   * In order to prevent extra allocations we use a value class (`extends AnyVal`) but in order
   * for this to be a zero allocation it must take at least 1 value parameter. Hence the `dummy`
   * Boolean which is never used.
   *
   * @param dummy
   * @tparam F
   * @tparam M
   */
  final class PartiallyAppliedPureApply[F[_], M](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](a: A): Program.Pure[F, M, A] = Program.Pure(a)
  }

  /**
   * Converts an `F[Program[F, M, A]]` to a `Program[F, M, A]` by flattening the outer structure
   * into the free monad
   *
   * @param fp
   * @tparam F
   * @tparam M
   * @tparam A
   * @return
   */
  def roll[F[_], M, A](fp: F[Program[F, M, A]]): Program[F, M, A] =
    Suspend(fp).flatMap(identity)

  object syntax {

    implicit class ToMeta[F[_], A](fa: F[A]) {
      def meta[M](m: M): Program[F, M, A] =
        Suspend(fa, Some(m))
    }

    implicit def wrap[F[_], M, A](fa: F[A]): Program[F, M, A] =
      Suspend(fa)

    // Not sure if this is safe to do
    implicit def unwrap[F[_]: Monad, M, A](p: Program[F, M, A]): F[A] =
      p.run
  }

  implicit def functorInstance[F[_], M]: Functor[Program[F, M, *]] =
    new Functor[Program[F, M, *]] {
      override def map[A, B](fa: Program[F, M, A])(f: A => B): Program[F, M, B] =
        fa.map(f)
    }

  implicit def applicativeInstance[F[_], M]: Functor[Program[F, M, *]] =
    new Applicative[Program[F, M, *]] {

      override def pure[A](x: A): Program[F, M, A] =
        Program.pure[F, M](x)

      // TODO should this be implemented without flatMap?
      override def ap[A, B](ff: Program[F, M, A => B])(fa: Program[F, M, A]): Program[F, M, B] =
        ff.flatMap(f => fa.map(f))
    }

  implicit def monadInstance[F[_], M]: Monad[Program[F, M, *]] =
    new StackSafeMonad[Program[F, M, *]] {

      override def flatMap[A, B](fa: Program[F, M, A])(f: A => Program[F, M, B]): Program[F, M, B] =
        fa.flatMap(f)

      override def pure[A](x: A): Program[F, M, A] = Program.pure[F, M](x)
    }

  implicit def monadErrorInstance[F[_], M, E](implicit ev: MonadError[F, E]): MonadError[Program[F, M, *], E] =
    new MonadError[Program[F, M, *], E] with StackSafeMonad[Program[F, M, *]] {

      override def flatMap[A, B](fa: Program[F, M, A])(f: A => Program[F, M, B]): Program[F, M, B] =
        fa.flatMap(f)

      override def raiseError[A](e: E): Program[F, M, A] =
        Suspend(ev.raiseError(e))

      // TODO
      override def handleErrorWith[A](fa: Program[F, M, A])(f: E => Program[F, M, A]): Program[F, M, A] = ???

      override def pure[A](x: A): Program[F, M, A] =
        Program.pure[F, M](x)
    }

  implicit def foldableInstance[F[_], M](implicit ev: Foldable[F]): Foldable[Program[F, M, *]] =
    new Foldable[Program[F, M, *]] {

      override def foldLeft[A, B](fa: Program[F, M, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: Program[F, M, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)
    }

  implicit def traverseInstance[F[_], M](implicit ev: Traverse[F]): Traverse[Program[F, M, *]] =
    new Traverse[Program[F, M, *]] {

      override def traverse[G[_], A, B](fa: Program[F, M, A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[Program[F, M, B]] =
        fa.traverse(f)

      override def foldLeft[A, B](fa: Program[F, M, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: Program[F, M, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]=
        fa.foldRight(lb)(f)
    }
}

