package wolfendale.example.controllers

import cats.data.Chain
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._
import wolfendale.control.{Machine, Program}
import wolfendale.example.Identifier
import wolfendale.example.actions.{FormActionRefinerBuilder, JourneyActionTransformer}

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class IndexController @Inject() (
                                  cc: ControllerComponents,
                                  formAction: FormActionRefinerBuilder,
                                  journeyAction: JourneyActionTransformer,
                                )(implicit ec: ExecutionContext) extends AbstractController(cc) {

  private def fallback(current: Program[IO, Identifier, Unit], history: Chain[Program[IO, Identifier, Unit]], e: Throwable): IO[Result] =
    current.meta.orElse(history.reverse.find(_.meta.isDefined).flatMap(_.meta)).map { identifier =>
      IO.pure(Redirect(routes.IndexController.backOnTrack(identifier.call.url)))
    }.getOrElse(IO.raiseError(e))

  private def get(identifier: Identifier, amend: Boolean): Action[AnyContent] =
    journeyAction.async { implicit request =>
      request.journey.runWith(Machine.runUntil(identifier)).flatMap { case (current, (history, result)) =>
        result match {
          case Left(e) =>
            fallback(current, history, e)
          case _ =>
            IO.pure(Ok(views.html.get(identifier, amend)))
        }
      }.unsafeToFuture()
    }

  private def post(identifier: Identifier, amend: Boolean): Action[AnyContent] = {

    val form = Form(
      single("value" -> text)
    )

    formAction(identifier, form).async { implicit request =>
      request.journey.runWith(for {
        _    <- Machine.runUntil[IO, Identifier, Throwable, Unit](identifier)
        _    <- Machine.step[IO, Identifier, Throwable, Unit]
        next <- Machine.resume[IO, Identifier, Throwable, Unit]
      } yield next).flatMap { case (current, (history, result)) =>
        result match {
          case Right(p) =>
            p.meta.map { identifier =>
              val url = if (amend) routes.IndexController.getCheckYourAnswers() else identifier.call
              IO.pure(Redirect(url).withSession(request.session))
            }.getOrElse(fallback(current, history, new IllegalStateException))
          case Left(e) => fallback(current, history, e)
        }
      }.unsafeToFuture()
    }
  }

  def getCheckYourAnswers: Action[AnyContent] =
    journeyAction.async { implicit request =>
      request.journey.runWith(Machine.runUntil(Identifier.CheckYourAnswers)).flatMap { case (current, (history, result)) =>
        result match {
          case Left(e) =>
            fallback(current, history, e)
          case _ =>
            val answers = for {
              program    <- history.toList
              identifier <- program.meta
              value      <- request.session.get(identifier.sessionKey)
            } yield (identifier, value)
            IO.pure(Ok(views.html.checkYourAnswers(answers)))
        }
      }.unsafeToFuture()
    }

  def getA(amend: Boolean): Action[AnyContent] = get(Identifier.A, amend)
  def postA(amend: Boolean): Action[AnyContent] = post(Identifier.A, amend)

  def getB(amend: Boolean): Action[AnyContent] = get(Identifier.B, amend)
  def postB(amend: Boolean): Action[AnyContent] = post(Identifier.B, amend)

  def getC(amend: Boolean): Action[AnyContent] = get(Identifier.C, amend)
  def postC(amend: Boolean): Action[AnyContent] = post(Identifier.C, amend)

  // This should be validated to make sure it's a safe url to redirect someone to
  def backOnTrack(url: String): Action[AnyContent] = Action { implicit request =>
    Ok(views.html.backOnTrack(url))
  }

  def clearSession: Action[AnyContent] = Action { implicit request =>
    Ok.withNewSession
  }
}
