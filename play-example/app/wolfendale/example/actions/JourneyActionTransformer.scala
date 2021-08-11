package wolfendale.example.actions

import play.api.mvc._
import wolfendale.example.requests.JourneyRequest

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class JourneyActionTransformer @Inject() (
                                           cc: ControllerComponents
                                         ) extends ActionBuilder[JourneyRequest, AnyContent] with ActionTransformer[Request, JourneyRequest] {

  override protected def transform[A](request: Request[A]): Future[JourneyRequest[A]] =
    Future.successful(JourneyRequest(request))

  override val parser: BodyParser[AnyContent] = cc.parsers.defaultBodyParser
  override protected val executionContext: ExecutionContext = cc.executionContext
}
