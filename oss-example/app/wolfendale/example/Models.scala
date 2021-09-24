package wolfendale.example

import play.api.data.FormError
import play.api.data.format.Formatter
import play.api.data.format.Formats._

import scala.util.control.NoStackTrace

case object PrematureExit extends Throwable with NoStackTrace

final case class ReturnsPeriod(value: Int)
object ReturnsPeriod {
  implicit val formatter: Formatter[ReturnsPeriod] =
    new Formatter[ReturnsPeriod] {
      val inner: Formatter[Int] = implicitly
      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], ReturnsPeriod] = inner.bind(key, data).map(ReturnsPeriod(_))
      override def unbind(key: String, value: ReturnsPeriod): Map[String, String] = inner.unbind(key, value.value)
    }
}

final case class Country(value: String)
object Country {
  implicit val formatter: Formatter[Country] =
    new Formatter[Country] {
      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Country] = data.get(key).toRight(Seq.empty).map(Country(_))
      override def unbind(key: String, value: Country): Map[String, String] = Map(key -> value.value)
    }
}

final case class VatRate(value: Int)
object VatRate {
  implicit val formatter: Formatter[VatRate] =
    new Formatter[VatRate] {
      val inner: Formatter[Int] = implicitly
      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], VatRate] = inner.bind(key, data).map(VatRate(_))
      override def unbind(key: String, value: VatRate): Map[String, String] = inner.unbind(key, value.value)
    }
}

final case class SalesDetails(sales: Int, vat: Int)
final case class NiDetails(value: Map[Country, Map[VatRate, SalesDetails]])
final case class EuDetails(value: Map[(Country, Country),Map[VatRate, SalesDetails]])

final case class ReturnDetails(period: ReturnsPeriod, niDetails: Option[NiDetails])

