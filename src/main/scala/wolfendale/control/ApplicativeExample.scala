package wolfendale.control

import cats._
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import wolfendale.control.syntax._

object ApplicativeExample extends IOApp {

  private val data: Map[String, String] = Map("c" -> "foo")

  private def get(key: String): Program[IO, String, String] =
    annotatedWith(key) {
      data.get(key)
        .map(IO.pure)
        .getOrElse(IO.raiseError(new Exception(s"$key not found")))
    }

  private val ab = (get("a"), get("b")).tupled
  private val cd = (get("c"), get("d")).tupled
  private val program = (ab, cd).tupled

  // Is there any way to get this to show the deepest next point?
  override def run(args: List[String]): IO[ExitCode] = for {
    (current, (history, result)) <- program.runWith(Machine.run)
    _                            <- IO.delay(println(current))
    _                            <- IO.delay(println(history))
  } yield ExitCode.Success
}
