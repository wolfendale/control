package wolfendale.example.requests

import play.api.mvc.{Request, Session, WrappedRequest}
import wolfendale.example.Journey

final case class JourneyRequest[A](
                                    request: Request[A],
                                    addingToSession: Map[String, String] = Map.empty
                                  ) extends WrappedRequest[A](request) {

  override val session: Session = request.session ++ addingToSession

  val journey: Journey = new Journey(session)
}
