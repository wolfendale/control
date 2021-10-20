package wolfendale.control

import scala.concurrent.{Await, Future}
import scala.util.Try
import cats._
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

object AddressHistoryExample2 extends App {

  abstract class Id(val sessionKey: String) extends Journey.Id

  abstract class Page(override val id: Id) extends Journey.Page

  case object CheckYourAnswersId extends Journey.Id
  case object CheckYourAnswersPage extends Journey.Page {
    val id: Journey.Id = CheckYourAnswersId
  }

  final case class AddressHistoryId(index: Int) extends Id(s"address-history-$index")
  final case class AddressHistoryPage(index: Int, followingAddress: Option[String]) extends Page(AddressHistoryId(index))

  final case class YearsId(index: Int) extends Id(s"years-$index")
  final case class YearsPage(index: Int, address: String) extends Page(YearsId(index))

  final case class AddressHistory(address: String, years: Int)

  val session: Map[String, String] = Map(
    "address-history-0" -> "foo",
    "years-0" -> "3"
  )

  def get(page: Page): Journey[String] = Journey(Future.fromTry(Try(session(page.id.sessionKey))), Some(page))

  def address(index: Int, followingAddress: Option[String]): Journey[AddressHistory] = for {
    address       <- get(AddressHistoryPage(index, followingAddress))
    timeAtAddress <- get(YearsPage(index, address))
  } yield AddressHistory(address, timeAtAddress.toInt)

  val addressHistory: Journey[List[AddressHistory]] = List.empty[AddressHistory].iterateUntilM { addresses =>
    address(addresses.length, addresses.headOption.map(_.address)).map(_ :: addresses)
  } { _.foldLeft(0)(_ + _.years) >= 5 }

  val journey: Journey[List[AddressHistory]] = for {
    addressHistory <- addressHistory
    _              <- Journey.page(CheckYourAnswersPage)
  } yield addressHistory

  val step = Await.result(journey.nextFor(AddressHistoryId(0)), 1.second)

  println(step)
}
