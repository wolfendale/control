package wolfendale.control

import cats._
import cats.data.{Chain, NonEmptyChain, StateT}
import cats.implicits._

import scala.language.implicitConversions

/**
 * This is essentially a `FreeT` monad transformer which also provides meta data for
 * different steps within the program, allowing us to write interpreters which use
 * that meta data to determine how the program should be executed
 *
 * TODO do we need to clear annotations when we wrap with things like flatMap?
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
      case Pure(a, m)           => f(a).map(Pure(_, m))
      case Suspend(fa, m)       => fa.traverse(f).map(Suspend(_, m)) // TODO do we need to retain annotation info here?
      case c: Continue[F, M, A] => c.nested match {
        case Pure(x, _)     => c.f(x).traverse(f)
        case Suspend(fx, m) => fx.traverse(x => c.f(x).traverse(f)).map(roll(_, m)) // TODO does we need to retain annotation info here?
      }
    }

  // TODO test, this was a nightmare
  final def handleErrorWith[E](f: E => Program[F, M, A])(implicit ev: ApplicativeError[F, E]): Program[F, M, A] =
    this match {
      case p: Pure[F, M, A]     => p
      case Suspend(fa, m)       => roll(fa.map(pure[F, M](_)).handleErrorWith(e => ev.pure(f(e))), m)
      case c: Continue[F, M, A] => roll(c.step.map(_.handleErrorWith(f)).handleErrorWith(e => f(e).step), c.meta)
    }

  final def isDone: Boolean =
    this match {
      case _: Result[F, M, A]   => true
      case _: Continue[F, M, A] => false
    }

  /**
   * Takes a single step in the program
   *
   * @param ev
   * @return The next step in the program, susepended in `F`
   */
  final def step(implicit ev: Applicative[F]): F[Program[F, M, A]] =
    this match {
      case r: Result[F, M, A]   => r.resolve(Pure(_))
      case c: Continue[F, M, A] => c.continue
    }

  /**
   * Attempts to take a step in the program, on failure the error
   * is caught from the inner `F` and returned
   *
   * @param ev
   * @tparam E the error type
   * @return returns the next step in the program, or the current step if it failed, plus an
   *         option containing the error if there is one
   */
  final def attemptStep[E](implicit ev: ApplicativeError[F, E]): F[(Program[F, M, A], Option[E])] =
    this.step.map((_, Option.empty[E])).handleErrorWith(e => ev.pure((this, Some(e))))

  /**
   * Runs the program to completion, this does not attempt to catch any errors,
   * any failures in `F` will be kept there
   *
   * @param ev
   * @return the result of running the inner program
   */
  final def run(implicit ev: Monad[F]): F[A] = {

    def next(p: Program[F, M, A]): F[Either[Program[F, M, A], A]] =
      p match {
        case r: Result[F, M, A]   => r.resolve(Right(_))
        case c: Continue[F, M, A] => c.continue.map(Left(_))
      }

    Monad[F].tailRecM(this)(next)
  }

  /**
   * Continually steps through the program while some predicate on the meta
   * data of each step holds true, or until the end of the program.
   *
   * @param f
   * @param ev
   * @return the step which the program got to while running, this might be
   *         the last step in a program if the predicate held true for the duration
   */
  final def runWhile(f: M => Boolean)(implicit ev: Monad[F]): F[Program[F, M, A]] =
    Monad[F].iterateWhileM(this)(_.step)(p => p.meta.forall(f) || p.isDone)

  /**
   *
   * Continually steps through the program until some pedicate on the meta
   * data of any step holds true, or until the end of the program.
   *
   * @param f
   * @param ev
   * @return the step which the program got to while running, this might be
   *         the last step in a program if the predicate never held true for the duration
   */
  final def runUntil(f: M => Boolean)(implicit ev: Monad[F]): F[Program[F, M, A]] =
    Monad[F].iterateUntilM(this)(_.step)(p => p.meta.forall(f) || p.isDone)

  final def runTo(f: M => Boolean)(implicit ev: Monad[F]): F[Program[F, M, A]] =
    Machine[F, M, A] { b =>
      b.runUntil(f) >> b.step
    }.runS(this)

  /**
   * Runs the program to completion collecting every step of the program along the way
   * in a `NonEmptyChain` (as this always includes the current step)
   *
   * @param ev
   * @return the last step of the program and the `NonEmptyChain` of program steps
   *         it took to get there
   */
  final def collect(implicit ev: Monad[F]): F[(Program[F, M, A], NonEmptyChain[Program[F, M, A]])] = {

    def next(steps: NonEmptyChain[Program[F, M, A]]): F[Either[NonEmptyChain[Program[F, M, A]], (Program[F, M, A], NonEmptyChain[Program[F, M, A]])]] = {
      val current = steps.last
      if (current.isDone) {
        ev.pure(Right((steps.last, steps)))
      } else {
        current.step.map(next => Left(steps :+ next))
      }
    }

    Monad[F].tailRecM(NonEmptyChain.one(this))(next)
  }

  /**
   * Continually steps through the program collecting every step taken along the way
   * while a predicate on the meta data of each step holds true
   *
   * @param f
   * @param ev
   * @return the step that the program got to while running, this might be the last
   *         step if the predicate held true for the duration of the program. Also,
   *         the `Chain` of steps it took to get there
   */
  final def collectWhile(f: M => Boolean)(implicit ev: Monad[F]): F[(Program[F, M, A], Chain[Program[F, M, A]])] = {

    def next(steps: Chain[Program[F, M, A]]): F[Either[Chain[Program[F, M, A]], (Program[F, M, A], Chain[Program[F, M, A]])]] = {
      steps.lastOption.map(_.step).getOrElse(ev.pure(this)).map { current =>
        val nextSteps = if (current.meta.forall(f)) steps :+ current else steps
        if (!current.meta.forall(f) || current.isDone) Right((current, nextSteps)) else Left(nextSteps)
      }
    }

    Monad[F].tailRecM(Chain.empty[Program[F, M, A]])(next)
  }

  /**
   * Continually steps through the program collecting every step taken along the way
   * until a predicate on the meta data of a step holds true
   *
   * @param f
   * @param ev
   * @return the step that the program got to while running, this might be the last
   *         step if the predicate never held true for the duration of the program.
   *         Also, the `Chain` of steps it took to get there
   */
  final def collectUntil(f: M => Boolean)(implicit ev: Monad[F]): F[(Program[F, M, A], Chain[Program[F, M, A]])] =
    collectWhile(!f(_))

  final def collectTo(f: M => Boolean)(implicit ev: Monad[F]): F[(Program[F, M, A], Chain[Program[F, M, A]])] =
    Machine[F, M, A] { b =>
      for {
        steps <- b.collectUntil(f)
        next  <- b.step
      } yield if (steps.lastOption.contains(next)) steps else steps :+ next
    }.run(this)

  /**
   * Attempt to run the program to completion
   *
   * @param ev
   * @tparam E
   * @return The step that the program got to while attempting to run, this will either
   *         be the last step in the program if there were no errors, or the step
   *         in the program which threw the error. Also, either the result of the
   *         program if it was successful or the error thrown if it was not
   */
  final def attempt[E](implicit ev: MonadError[F, E]): F[(Program[F, M, A], Either[E, A])] = {

    def next(p: Program[F, M, A]): F[Either[Program[F, M, A], (Program[F, M, A], Either[E, A])]] =
      p match {
        case r: Result[F, M, A]   => r.resolve(a => Right((r, Right(a))))
        case c: Continue[F, M, A] => c.attemptStep[E].map({ case (p, oe) =>
          oe.map(e => Right((p, Left(e))))
            .getOrElse(Left(p))
        })
      }

    Monad[F].tailRecM(this)(next)
  }

  final def attemptWhile[E](f: M => Boolean)(implicit ev: MonadError[F, E]): F[(Program[F, M, A], Option[E])] = {

    def next(p: Program[F, M, A]): F[Either[Program[F, M, A], (Program[F, M, A], Option[E])]] =
      p match {
        case r: Result[F, M, A]   => r.resolve(_ => Right((r, None)))
        case c: Continue[F, M, A] => c.attemptStep[E].map({ case (p, oe) =>
          oe.map(e => Right((p, Some(e))))
            .getOrElse {
              if (p.meta.forall(f)) Left(p) else Right(p, None)
            }
        })
      }

    Monad[F].tailRecM(this)(next)
  }

  final def attemptUntil[E](f: M => Boolean)(implicit ev: MonadError[F, E]): F[(Program[F, M, A], Option[E])] =
    attemptWhile(!f(_))

  final def attemptTo[E](f: M => Boolean)(implicit ev: MonadError[F, E]): F[(Program[F, M, A], Option[E])] =
    Machine[F, M, A] { b =>
      b.attemptUntil(f) >> b.attemptStep
    }.run(this)

  final def attemptCollect[E](implicit ev: MonadError[F, E]): F[(Program[F, M, A], (NonEmptyChain[Program[F, M, A]], Option[E]))] = {

    def next(steps: NonEmptyChain[Program[F, M, A]]): F[Either[NonEmptyChain[Program[F, M, A]], (Program[F, M, A], (NonEmptyChain[Program[F, M, A]], Option[E]))]] = {
      val current = steps.last
      if (current.isDone) {
        ev.pure(Right((steps.last, (steps, None))))
      } else {
        current.attemptStep.map { case (p, oe) =>
          oe.map(e => Right((p, (steps, Some(e)))))
            .getOrElse(Left(steps :+ p))
        }
      }
    }

    Monad[F].tailRecM(NonEmptyChain.one(this))(next)
  }

  final def attemptCollectWhile[E](f: M => Boolean)(implicit ev: MonadError[F, E]): F[(Program[F, M, A], (Chain[Program[F, M, A]], Option[E]))] = {

    def next(steps: Chain[Program[F, M, A]]): F[Either[Chain[Program[F, M, A]], (Program[F, M, A], (Chain[Program[F, M, A]], Option[E]))]] = {
      steps.lastOption.map(_.attemptStep).getOrElse(ev.pure((this, None))).map { case (current, oe) =>
        val nextSteps = if (current.meta.forall(f)) steps :+ current else steps
        oe.map(e => Right((current, (nextSteps, Some(e)))))
          .getOrElse {
            if (!current.meta.forall(f) || current.isDone) {
              Right((current, (nextSteps, None)))
            } else {
              Left(nextSteps)
            }
          }
      }
    }

    Monad[F].tailRecM(Chain.empty[Program[F, M, A]])(next)
  }

  final def attemptCollectUntil[E](f: M => Boolean)(implicit ev: MonadError[F, E]): F[(Program[F, M, A], (Chain[Program[F, M, A]], Option[E]))] =
    attemptCollectWhile(!f(_))

  final def attemptCollectTo[E](f: M => Boolean)(implicit ev: MonadError[F, E]): F[(Program[F, M, A], (Chain[Program[F, M, A]], Option[E]))] =
    Machine[F, M, A] { b =>
      for {
        (steps, e1) <- b.attemptCollectUntil(f)
        e2          <- b.attemptStep
        next        <- StateT.get[F, Program[F, M, A]]
        nextSteps   =  if (steps.lastOption.contains(next)) steps else steps :+ next
      } yield (nextSteps, e1 orElse e2)
    }.run(this)
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
  sealed abstract class Continue[F[_], M, A] extends Program[F, M, A] with Serializable {

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
    def apply[A](a: A): Program[F, M, A] = Pure(a)
  }

  def raiseError[E]: PartiallyAppliedRaiseError[E] = new PartiallyAppliedRaiseError[E]

  final class PartiallyAppliedRaiseError[E](private val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], M, A](e: E)(implicit ev: MonadError[F, E]): Program[F, M, A] =
      Suspend(ev.raiseError(e))
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
  def roll[F[_], M, A](fp: F[Program[F, M, A]], meta: Option[M] = None): Program[F, M, A] =
    Continue(Suspend(fp), identity[Program[F, M, A]], meta)

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

  implicit def monadErrorInstance[F[_], M, E](implicit ev: ApplicativeError[F, E]): MonadError[Program[F, M, *], E] =
    new MonadError[Program[F, M, *], E] with StackSafeMonad[Program[F, M, *]] {

      override def flatMap[A, B](fa: Program[F, M, A])(f: A => Program[F, M, B]): Program[F, M, B] =
        fa.flatMap(f)

      override def raiseError[A](e: E): Program[F, M, A] =
        Suspend(ev.raiseError(e))

      // TODO test, this was a nightmare
      override def handleErrorWith[A](pfa: Program[F, M, A])(f: E => Program[F, M, A]): Program[F, M, A] =
        pfa.handleErrorWith(f)

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

