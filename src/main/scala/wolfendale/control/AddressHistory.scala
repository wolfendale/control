package wolfendale.control

import cats._
import cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import wolfendale.control.machine.Builder
import wolfendale.control.syntax._

import scala.io.StdIn

object AddressHistory extends App {

  final case class AddressHistory(address: String, years: Int)

  def getAddress(followingAddress: Option[String]): Task[String] =
    Task.eval {
      val message = followingAddress.map { a =>
        s"What was your address before $a?"
      }.getOrElse("What is your current address?")
      println(message)
      val answer = StdIn.readLine()
      println(s"($answer)")
      answer
    }

  def howLong(address: String): Task[Int] =
    Task.eval {
      println(s"How many years have you lived at $address?")
      val answer = StdIn.readLine().toInt
      println(s"($answer)")
      answer
    }

  def address(index: Int, followingAddress: Option[String]) = for {
    address       <- getAddress(followingAddress) @@ s"address-$index"
    timeAtAddress <- howLong(address) @@ s"years-$index"
  } yield AddressHistory(address, timeAtAddress)

  val program = List.empty[AddressHistory].iterateUntilM { addresses =>
    println(addresses)
    address(addresses.length, addresses.headOption.map(_.address)).map(_ :: addresses)
  } { addresses =>
    addresses.foldLeft(0)(_ + _.years) >= 5
  }

  val result = Builder
    .annotatedWith[String]
    .toCompletion
    .trace[Task, Throwable, List[AddressHistory]]
    .run(program)
    .runSyncUnsafe()

  println(result)
}
