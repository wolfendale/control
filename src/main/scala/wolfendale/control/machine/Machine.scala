package wolfendale.control.machine

import cats._
import io.iteratee.Iteratee
import wolfendale.control.Program

abstract class Machine[F[_], T, E, A, O] extends Serializable {

  def sink: Iteratee[F, Program.Step[F, T, E, A], O]

  final def run(program: Program[F, T, E, A])(implicit M: MonadError[F, E]): F[O] =
    program.enumerate.into(sink)
}
