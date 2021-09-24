package wolfendale.example

import cats.Monad
import cats.data.{Chain, NonEmptyChain}
import cats.effect.IO
import cats.implicits._
import play.api.data.FormError
import play.api.data.format.Formatter
import play.api.mvc.Session
import wolfendale.control.syntax._
import wolfendale.control.{Machine, Program}

class Journey(session: Session) {

  import play.api.data.format.Formats._

  implicit def listFormatter[A](implicit ev: Formatter[A]): Formatter[List[A]] =
    new Formatter[List[A]] {
      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], List[A]] =
        data.get(key).toRight(Seq.empty[FormError]).flatTraverse { str =>
          str.split(",").toList.map(s => ev.bind("", Map("" -> s)))
        }.sequence
      override def unbind(key: String, value: List[A]): Map[String, String] =
        Map(key -> value.map(a => ev.unbind("", a)("")).mkString(","))
    }

  def iterateWhileMM[F[_], A](init: A)(f: A => F[A])(p: A => F[Boolean])(implicit ev: Monad[F]): F[A] =
    ev.tailRecM(init) { a =>
      ev.ifM(p(a))(
        ev.map(f(a))(Left(_)),
        ev.pure(Right(a))
      )
    }

  def loopWhile[A](f: Int => Program[IO, Identifier, A])(p: Int => Program[IO, Identifier, Boolean]): Program[IO, Identifier, Vector[A]] =
    iterateWhileMM(Vector.empty[A])(list => f(list.length).map(list :+ _))(list => list.lastOption.as(p(list.length - 1)).getOrElse(Program.pure(true)))

  private def getFromSession[A](identifier: Identifier)(implicit ev: Formatter[A]): Program[IO, Identifier, A] = {
    annotatedWith[Identifier](identifier) {
      session.get(identifier.sessionKey)
        .flatMap { value =>
          implicitly[Formatter[A]].bind("value", Map("value" -> value)).toOption.map(IO.pure)
        }
        .getOrElse(IO.raiseError[A](new Exception(s"${identifier.sessionKey} not found")))
    }
  }

  // This would probably be an from an API call?
  val getReturnsPeriods: IO[NonEmptyChain[ReturnsPeriod]] =
    IO.pure(NonEmptyChain(ReturnsPeriod(1)))

  def getWhichPeriod(returnsPeriods: NonEmptyChain[ReturnsPeriod]): Program[IO, Identifier, ReturnsPeriod] =
    if (returnsPeriods.length == 1) {
      val period = returnsPeriods.head
      getFromSession[Boolean](Identifier.DoYouWantToStartPeriod).ifM(
        Program.pure(period),
        IO.raiseError[ReturnsPeriod](PrematureExit).annotated(Identifier.NoOtherReturnsAvailable)
      )
    } else {
      getFromSession[ReturnsPeriod](Identifier.WhichPeriod)
    }

  def getNiDetails: Program[IO, Identifier, Option[NiDetails]] = {

    def getSalesDetails(countryIndex: Int, rateIndex: Int): Program[IO, Identifier, SalesDetails] = for {
      sales <- getFromSession[Int](Identifier.NiTotalSales(countryIndex, rateIndex))
      vat   <- getFromSession[Int](Identifier.NiTotalVat(countryIndex, rateIndex))
    } yield SalesDetails(sales, vat)

    def getCountryDetails(countryIndex: Int): Program[IO, Identifier, (Country, Map[VatRate, SalesDetails])] = for {
      country <- getFromSession[Country](Identifier.NiCountry(countryIndex))
      rates   <- getFromSession[List[VatRate]](Identifier.NiVatRates(countryIndex))
      result  <- rates.zipWithIndex.traverse { case (rate, rateIndex) =>
        getSalesDetails(countryIndex, rateIndex).map(rate -> _)
      }
    } yield country -> result.toMap

    def getDetails: Program[IO, Identifier, NiDetails] =
      loopWhile(getCountryDetails) { countryIndex =>
        getFromSession[Boolean](Identifier.NiCheckYourAnswers(countryIndex))
      }.map(details => NiDetails(details.toMap))

    getFromSession[Boolean](Identifier.NiApplicable).ifM(
      getDetails.map(Some(_)),
      Program.pure(None)
    )
  }

  def getEuDetails: Program[IO, Identifier, Option[EuDetails]] = {

    def getSalesDetails(fromIndex: Int, toIndex: Int, rateIndex: Int): Program[IO, Identifier, SalesDetails] = for {
      sales <- getFromSession[Int](Identifier.EuTotalSales(fromIndex, toIndex, rateIndex))
      vat   <- getFromSession[Int](Identifier.EuTotalSales(fromIndex, toIndex, rateIndex))
    } yield SalesDetails(sales, vat)

    def getToDetails(from: Country, fromIndex: Int, toIndex: Int): Program[IO, Identifier, ((Country, Country), Map[VatRate, SalesDetails])] = for {
      to     <- getFromSession[Country](Identifier.EuCountryTo(fromIndex, toIndex))
      rates  <- getFromSession[List[VatRate]](Identifier.EuVatRates(fromIndex, toIndex))
      result <- rates.zipWithIndex.traverse { case (rate, rateIndex) =>
                  getSalesDetails(fromIndex, toIndex, rateIndex).map(rate -> _)
                }
    } yield (from, to) -> result.toMap

    def getDetails: Program[IO, Identifier, EuDetails] =
      loopWhile { fromIndex =>
        getFromSession[Country](Identifier.EuCountryFrom(fromIndex)).flatMap { from =>
          loopWhile(getToDetails(from, fromIndex, _)) { toIndex =>
            getFromSession[Boolean](Identifier.EuAddMoreTo(fromIndex, toIndex))
          }
        }.map(_.toMap)
      } { fromIndex =>
        getFromSession[Boolean](Identifier.EuAddMoreFrom(fromIndex))
      }.map(details => EuDetails(details.reduce(_ ++ _)))

    getFromSession[Boolean](Identifier.EuApplicable).ifM(
      getDetails.map(Some(_)),
      Program.pure(None)
    )
  }

  val program: Program[IO, Identifier, Unit] = for {
    returnsPeriods <- getReturnsPeriods.toProgram
    period         <- getWhichPeriod(returnsPeriods)
    niDetails      <- getNiDetails
    euDetails      <- getEuDetails
    _              <- IO.unit.annotated[Identifier](Identifier.CheckYourAnswers)
  } yield ()

  def runWith[B](machine: Machine[IO, Identifier, Unit, Throwable, B]): IO[(Program[IO, Identifier, Unit], (Chain[Program[IO, Identifier, Unit]], Either[Throwable, B]))] =
    program.runWith(machine)
}
