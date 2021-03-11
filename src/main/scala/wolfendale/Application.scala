package wolfendale

import cats._
import cats.data._
import cats.implicits._
import io.iteratee._
import monix.eval.Task
import wolfendale.control.Program
import wolfendale.control.machine.Builder
import wolfendale.control.syntax._

object Application extends App {

  import monix.execution.Scheduler.Implicits.global

  val program = for {
    a <- Task.eval(1) @@ "foo"
    b <- Task.eval(2) @@ None
    c <- Task.eval(3) @@ "baz"
    d <- Task.eval(4) @@ "quux"
  } yield a + b + c + d

  val machine1 = Builder
    .annotatedWith[String]
    .whilst(_ != "quux")

  val machine2 = Builder
    .annotatedWith[String]
    .whilst(_ != "quux")
    .step
    .step

  val result1 = machine1.runTrace(program)
  val result2 = machine2.runTrace(program)

  println(result1.runSyncUnsafe())
  println(result2.runSyncUnsafe())
}
