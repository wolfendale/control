package wolfendale.control

import cats.effect._
import wolfendale.control.syntax._

object V2App extends App {

  val program = for {
    a <- IO.pure(1).annotated("foo")
    b <- IO.pure(2).toProgram
    _ <- IO.raiseError[Unit](new Exception("Hi")).annotated("boom")
    c <- IO.pure(3).annotated("baz")
  } yield a + b + c


  val (current, (history, result)) = program.runWith(Machine.runUntil("boom")).unsafeRunSync()
  println("current", current)
  println("result", result)
  println("history", history)

}
