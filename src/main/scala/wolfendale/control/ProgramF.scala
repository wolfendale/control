//package wolfendale.control
//
//import cats._
//import cats.implicits._
//import higherkindness.droste.{Algebra, Basis, Coalgebra}
//
//sealed abstract class ProgramF[F[_], T, A, C]
//
//object ProgramF {
//
//  final case class PureF[F[_], T, A, C](a: A, annotation: Option[T]) extends ProgramF[F, T, A, C]
//  final case class SuspendF[F[_], T, A, C](fa: F[A], annotation: Option[T]) extends ProgramF[F, T, A, C]
//  final case class ContinueF[F[_], T, A, C](next: F[C], annotation: Option[T]) extends ProgramF[F, T, A, C]
//
//  implicit def functorInstance[F[_], T, A](implicit F: Functor[F]): Functor[ProgramF[F, T, A, *]] =
//    new Functor[ProgramF[F, T, A, *]] {
//      override def map[C, B](fa: ProgramF[F, T, A, C])(f: C => B): ProgramF[F, T, A, B] =
//        fa match {
//          case ContinueF(next, t) => ContinueF(next.map(f), t)
//          case other              => other.asInstanceOf[ProgramF[F, T, A, B]]
//        }
//    }
//
//  private def embed[F[_], T, A](implicit M: Monad[F]): Algebra[ProgramF[F, T, A, *], F[Program[F, T, A]]] =
//    Algebra {
//      case PureF(a, t)                                => M.pure(Program.Pure(a, t))
//      case SuspendF(fa, t)                            => M.pure(Program.Suspend(fa, t))
//      case c: ContinueF[F, T, A, F[Program[F, T, A]]] => c.next.flatten
//    }
//
//  private def project[F[_], T, A](implicit M: Monad[F]): Coalgebra[ProgramF[F, T, A, *], F[Program[F, T, A]]] =
//    Coalgebra { _.map {
//      case Program.Pure(a, t)           => PureF[F, T, A, Program[F, T, A]](a, t)
//      case Program.Suspend(fa, t)       => SuspendF[F, T, A, Program[F, T, A]](fa, t)
//      case c: Program.Continue[F, T, A] => ContinueF[F, T, A, Program[F, T, A]](c.step, c.annotation)
//    } }
//
//  implicit def basisInstance[F[_], T, A](implicit A: Applicative[F]): Basis[ProgramF[F, T, A, *], F[Program[F, T, A]]] =
//    Basis.Default(embed, project)
//}
