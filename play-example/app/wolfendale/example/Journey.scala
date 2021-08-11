package wolfendale.example

import cats.data.Chain
import cats.effect.IO
import play.api.mvc.Session
import wolfendale.control._
import wolfendale.control.syntax._

class Journey(session: Session) {

  private def getFromSession(identifier: Identifier): Program[IO, Identifier, String] = {
    annotatedWith[Identifier](identifier) {
      session.get(identifier.sessionKey)
        .map(IO.pure)
        .getOrElse(IO.raiseError[String](new Exception(s"${identifier.sessionKey} not found")))
    }
  }

  val program: Program[IO, Identifier, Unit] = for {
    a <- getFromSession(Identifier.A)
    b <- getFromSession(Identifier.B)
    c <- getFromSession(Identifier.C)
    _ <- IO.unit.annotated[Identifier](Identifier.CheckYourAnswers)
  } yield ()

  def runWith[B](machine: Machine[IO, Identifier, Unit, Throwable, B]): IO[(Program[IO, Identifier, Unit], (Chain[Program[IO, Identifier, Unit]], Either[Throwable, B]))] =
    program.runWith(machine)
}
