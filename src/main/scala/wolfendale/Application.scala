package wolfendale

import cats._
import cats.implicits._
import monix.eval.Task
import wolfendale.Program.Syntax._

object Application extends App {

  val program: Program[Eval, Int] = for {
    a <- Eval.now(1).blank
    b <- Eval.now(2).blank
  } yield a + b

//  println(program)
//
//  val Left(program2) = program.step.value
//  println(program2)
//
//  val Left(program3) = program2.step.value
//  println(program3)
//
//  val Right(result) = program3.step.value
//  println(result)

  val result = program.tailRecM {
    _.intercept {
      case a =>
        println(a)
        a
    }.step
  }.value
}
