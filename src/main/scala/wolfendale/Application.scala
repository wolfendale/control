package wolfendale

import cats._
import cats.data._
import cats.implicits._
import io.iteratee._
import wolfendale.control._
import wolfendale.control.Program.Continue
import wolfendale.control.syntax._
import wolfendale.control.eval._

import scala.annotation.tailrec

object Application extends App {

  val program = for {
    a <- Option(1) @@ "foo"
    b <- Option(2) @@ "bar"
    c <- Option(3) @@ "baz"
    d <- Option(4) @@ "quux"
  } yield a + b + c + d

  val stream: Enumerator[Option, ProgramStep[Option, String, Unit, Int]] = enumerator(program)
  val take = Iteratee.take[Option, ProgramStep[Option, String, Unit, Int]](1)
  val machine = take combine take
  val result = stream.into(machine)

  println(result)
}
