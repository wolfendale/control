package wolfendale.example.actions

import play.api.data.Form
import play.api.mvc._
import wolfendale.example.Identifier
import wolfendale.example.requests.JourneyRequest

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class FormActionRefinerBuilder @Inject() (cc: ControllerComponents) {
  def apply(identifier: Identifier, form: Form[String]): FormActionRefiner =
    new FormActionRefiner(identifier, form, cc)
}

class FormActionRefiner(
                         identifier: Identifier,
                         form: Form[String],
                         cc: ControllerComponents
                       ) extends ActionBuilder[JourneyRequest, AnyContent] with ActionRefiner[Request, JourneyRequest] {

  override protected def refine[B](request: Request[B]): Future[Either[Result, JourneyRequest[B]]] = {
    form.bindFromRequest()(request, cc.parsers.formBinding(cc.parsers.DefaultMaxTextLength)).fold(
      _ => Future.successful(Left(Results.BadRequest("No"))),
      s => Future.successful(Right(JourneyRequest(request, Map(identifier.sessionKey -> s))))
    )
  }

  override val parser: BodyParser[AnyContent] = cc.parsers.defaultBodyParser
  override protected val executionContext: ExecutionContext = cc.executionContext
}
