package wolfendale.example.controllers

import cats.data.Chain
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import play.api.Logging
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
                                )(implicit ec: ExecutionContext) extends AbstractController(cc) with Logging {

  private def fallback(
                        current: Program[IO, Identifier, Unit],
                        history: Chain[Program[IO, Identifier, Unit]],
                        e: Throwable,
                        amend: Boolean = false
                      ): IO[Result] =
    current.meta.orElse(history.reverse.find(_.meta.isDefined).flatMap(_.meta)).map { identifier =>
      if (amend) {
        IO.pure(Redirect(identifier.call))
      } else {
        IO.pure(Redirect(routes.IndexController.backOnTrack(identifier.call.url)))
      }
    }.getOrElse(IO.raiseError(e))

  def get(identifier: Identifier, amend: Boolean): Action[AnyContent] =
    journeyAction.async { implicit request =>
      request.journey.runWith(Machine.runUntil(identifier)).flatMap { case (current, (history, result)) =>
        result match {
          case Left(e) =>
            fallback(current, history, e, amend)
          case _ =>
            IO.pure(Ok(views.html.get(identifier, request.session.get(identifier.sessionKey), amend)))
        }
      }.unsafeToFuture()
    }

  def post(identifier: Identifier, amend: Boolean): Action[AnyContent] = {

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

  def backOnTrack(url: String): Action[AnyContent] = Action { implicit request =>
    Ok(views.html.backOnTrack(url))
  }

  def clearSession: Action[AnyContent] = Action { implicit request =>
    Ok.withNewSession
  }

  def getWhichPeriod(amend: Boolean): Action[AnyContent] = get(Identifier.WhichPeriod, amend)
  def postWhichPeriod(amend: Boolean): Action[AnyContent] = post(Identifier.WhichPeriod, amend)

  def getDoYouWantToStartPeriod(amend: Boolean): Action[AnyContent] = get(Identifier.DoYouWantToStartPeriod, amend)
  def postDoYouWantToStartPeriod(amend: Boolean): Action[AnyContent] = post(Identifier.DoYouWantToStartPeriod, amend)

  def getNoOtherReturnsAvailable: Action[AnyContent] = get(Identifier.NoOtherReturnsAvailable, amend = false)

  def getNiApplicable(amend: Boolean): Action[AnyContent] = get(Identifier.NiApplicable, amend)
  def postNiApplicable(amend: Boolean): Action[AnyContent] = post(Identifier.NiApplicable, amend)

  def getNiCountry(country: Int, amend: Boolean): Action[AnyContent] = get(Identifier.NiCountry(country), amend)
  def postNiCountry(country: Int, amend: Boolean): Action[AnyContent] = post(Identifier.NiCountry(country), amend)

  def getNiVatRates(country: Int, amend: Boolean): Action[AnyContent] = get(Identifier.NiVatRates(country), amend)
  def postNiVatRates(country: Int, amend: Boolean): Action[AnyContent] = post(Identifier.NiVatRates(country), amend)

  def getNiTotalSales(country: Int, rate: Int, amend: Boolean): Action[AnyContent] = get(Identifier.NiTotalSales(country, rate), amend)
  def postNiTotalSales(country: Int, rate: Int, amend: Boolean): Action[AnyContent] = post(Identifier.NiTotalSales(country, rate), amend)

  def getNiTotalVat(country: Int, rate: Int, amend: Boolean): Action[AnyContent] = get(Identifier.NiTotalVat(country, rate), amend)
  def postNiTotalVat(country: Int, rate: Int, amend: Boolean): Action[AnyContent] = post(Identifier.NiTotalVat(country, rate), amend)

  def getNiCheckYourAnswers(country: Int): Action[AnyContent] = get(Identifier.NiCheckYourAnswers(country), amend = false)
  def postNiCheckYourAnswers(country: Int): Action[AnyContent] = post(Identifier.NiCheckYourAnswers(country), amend = false)

  def getEuApplicable(amend: Boolean): Action[AnyContent] = get(Identifier.EuApplicable, amend)
  def postEuApplicable(amend: Boolean): Action[AnyContent] = post(Identifier.EuApplicable, amend)

  def getEuCountryFrom(from: Int, amend: Boolean): Action[AnyContent] = get(Identifier.EuCountryFrom(from), amend)
  def postEuCountryFrom(from: Int, amend: Boolean): Action[AnyContent] = post(Identifier.EuCountryFrom(from), amend)

  def getEuCountryTo(from: Int, to: Int, amend: Boolean): Action[AnyContent] = get(Identifier.EuCountryTo(from, to), amend)
  def postEuCountryTo(from: Int, to: Int, amend: Boolean): Action[AnyContent] = post(Identifier.EuCountryTo(from, to), amend)

  def getEuVatRates(from: Int, to: Int, amend: Boolean): Action[AnyContent] = get(Identifier.EuVatRates(from, to), amend)
  def postEuVatRates(from: Int, to: Int, amend: Boolean): Action[AnyContent] = post(Identifier.EuVatRates(from, to), amend)

  def getEuTotalSales(from: Int, to: Int, rate: Int, amend: Boolean): Action[AnyContent] = get(Identifier.EuTotalSales(from, to, rate), amend)
  def postEuTotalSales(from: Int, to: Int, rate: Int, amend: Boolean): Action[AnyContent] = post(Identifier.EuTotalSales(from, to, rate), amend)

  def getEuTotalVat(from: Int, to: Int, rate: Int, amend: Boolean): Action[AnyContent] = get(Identifier.EuTotalVat(from, to, rate), amend)
  def postEuTotalVat(from: Int, to: Int, rate: Int, amend: Boolean): Action[AnyContent] = post(Identifier.EuTotalVat(from, to, rate), amend)

  def getEuCheckYourAnswers(from: Int, to: Int): Action[AnyContent] = get(Identifier.EuCheckYourAnswers(from, to), false)
  def postEuCheckYourAnswers(from: Int, to: Int): Action[AnyContent] = post(Identifier.EuCheckYourAnswers(from, to), false)

  def getEuAddMoreTo(from: Int, to: Int, amend: Boolean): Action[AnyContent] = get(Identifier.EuAddMoreTo(from, to), amend)
  def postEuAddMoreTo(from: Int, to: Int, amend: Boolean): Action[AnyContent] = post(Identifier.EuAddMoreTo(from, to), amend)

  def getEuAddMoreFrom(from: Int, amend: Boolean): Action[AnyContent] = get(Identifier.EuAddMoreFrom(from), amend)
  def postEuAddMoreFrom(from: Int, amend: Boolean): Action[AnyContent] = post(Identifier.EuAddMoreFrom(from), amend)
}
