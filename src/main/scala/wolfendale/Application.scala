package wolfendale

import cats._
import cats.data._
import cats.implicits._
import higherkindness.droste._
import wolfendale.control.Program
import wolfendale.control.Program.Continue
import wolfendale.control.syntax._

import scala.annotation.tailrec

object Application extends App {

  val program: Program[Eval, String, Int] = for {
    a <- Eval.now(1) @@ "foo"
    b <- Eval.now(2) @@ "bar"
    c <- Eval.now(3) @@ "baz"
  } yield a + b + c

  println(program)
  val Left(a) = program.step.value
  println(a)
  val Left(b) = a.step.value
  println(b)
  val Left(c) = b.step.value
  println(c)
  val Right(d) = c.step.value
  println(d)

  println(program.foldLeft(0)(_ - _))
}
