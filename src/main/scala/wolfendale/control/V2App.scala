package wolfendale.control

import cats.effect._
import wolfendale.control.syntax._

object V2App extends App {

  val program = for {
    a <- IO.pure(1).annotated("foo")
    b <- IO.pure(2).annotated("bar")
    _ <- IO.raiseError[Unit](new Exception("Hi")).toProgram[String]
    c <- IO.pure(3).annotated("baz")
  } yield a + b + c

  val machine = Machine[IO, String, Int] { b =>
    for {
      a <- b.attemptCollectUntil(_ == "baz")
    } yield a
  }

  val result = machine.runA(program).unsafeRunSync()

  println(result)
}
