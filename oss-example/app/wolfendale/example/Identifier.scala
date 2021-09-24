package wolfendale.example

import play.api.mvc.Call

sealed abstract class Identifier(val sessionKey: String, val call: Call)

object Identifier {

  case object WhichPeriod extends Identifier("which-period", controllers.routes.IndexController.getWhichPeriod())
  case object DoYouWantToStartPeriod extends Identifier("do-you-want-to-start-period", controllers.routes.IndexController.getDoYouWantToStartPeriod())
  case object NoOtherReturnsAvailable extends Identifier("no-other-returns-available", controllers.routes.IndexController.getNoOtherReturnsAvailable())

  case object NiApplicable extends Identifier("ni-applicable", controllers.routes.IndexController.getNiApplicable())
  final case class NiCountry(country: Int) extends Identifier(s"ni-country/$country", controllers.routes.IndexController.getNiCountry(country))
  final case class NiVatRates(country: Int) extends Identifier(s"ni-vat-rate/$country", controllers.routes.IndexController.getNiVatRates(country))
  final case class NiTotalSales(country: Int, rate: Int) extends Identifier(s"ni-total-sales/$country/$rate", controllers.routes.IndexController.getNiTotalSales(country, rate))
  final case class NiTotalVat(country: Int, rate: Int) extends Identifier(s"ni-total-vat/$country/$rate", controllers.routes.IndexController.getNiTotalVat(country, rate))
  final case class NiCheckYourAnswers(country: Int) extends Identifier(s"ni-check-your-answers/$country", controllers.routes.IndexController.getNiCheckYourAnswers(country))

  case object EuApplicable extends Identifier("eu-applicable", controllers.routes.IndexController.getEuApplicable())
  final case class EuCountryFrom(from: Int) extends Identifier(s"eu-from/$from", controllers.routes.IndexController.getEuCountryFrom(from))
  final case class EuCountryTo(from: Int, to: Int) extends Identifier(s"eu-to/$from/$to", controllers.routes.IndexController.getEuCountryTo(from, to))
  final case class EuVatRates(from: Int, to: Int) extends Identifier(s"eu-vat-rates/$from/$to", controllers.routes.IndexController.getEuVatRates(from, to))
  final case class EuTotalSales(from: Int, to: Int, rate: Int) extends Identifier(s"eu-total-sales/$from/$to/$rate", controllers.routes.IndexController.getEuTotalSales(from, to, rate))
  final case class EuTotalVat(from: Int, to: Int, rate: Int) extends Identifier(s"eu-total-vat/$from/$to/$rate", controllers.routes.IndexController.getEuTotalVat(from, to, rate))
  final case class EuCheckYourAnswers(from: Int, to: Int) extends Identifier(s"eu-check-your-answers/$from/$to", controllers.routes.IndexController.getEuCheckYourAnswers(from, to))
  final case class EuAddMoreTo(from: Int, to: Int) extends Identifier(s"eu-add-more-to/$from/$to", controllers.routes.IndexController.getEuAddMoreTo(from, to))
  final case class EuAddMoreFrom(from: Int) extends Identifier(s"eu-add-more-from/$from", controllers.routes.IndexController.getEuAddMoreFrom(from))

  object CheckYourAnswers extends Identifier("check-your-answers", controllers.routes.IndexController.getCheckYourAnswers())
}