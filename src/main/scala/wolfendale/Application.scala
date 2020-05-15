package wolfendale

import cats.Eval
import cats.implicits._
import higherkindness.droste.{Algebra, Coalgebra, RCoalgebra}
import io.iteratee._
import io.iteratee.modules.eval._
import wolfendale.control.ProgramF.PureF
import wolfendale.control.{Program, ProgramF}
import wolfendale.control.eval._
import wolfendale.control.syntax._

object Application extends App {

  val program: Program[Eval, String, Int] = for {
    a <- Eval.always(1) @@ "foo"
    b <- Eval.always(2) @@ "baz"
    c <- Eval.always(3) @@ "bar"
  } yield a + b + c

  program.enumerate.into(Iteratee.foreach(println)).value
}
