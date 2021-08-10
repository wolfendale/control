package wolfendale.control

import cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import wolfendale.control.syntax._

import scala.io.StdIn

object AddressHistory extends App {

  final case class AddressHistory(address: String, years: Int)

  sealed trait Identifier
  final case class AddressIdentifier(index: Int) extends Identifier
  final case class YearsIdentifier(index: Int) extends Identifier

  def getAddress(followingAddress: Option[String], index: Int): Program[Task, Identifier, String] =
    annotatedWith[Identifier](AddressIdentifier(index)) {
      Task.eval {

        val message = followingAddress.map { a =>
          s"What was your address before $a?"
        }.getOrElse("What is your current address?")

        println(message)
        val answer = StdIn.readLine()
        println(s"($answer)")
        answer
      }
    }

  def howLong(address: String, index: Int): Program[Task, Identifier, Int] =
    annotatedWith[Identifier](YearsIdentifier(index)) {
      Task.eval {
        println(s"How many years have you lived at $address?")
        val answer = StdIn.readLine().toInt
        println(s"($answer)")
        answer
      }
    }

  def address(index: Int, followingAddress: Option[String]): Program[Task, Identifier, AddressHistory] = for {
    address       <- getAddress(followingAddress, index)
    timeAtAddress <- howLong(address, index)
  } yield AddressHistory(address, timeAtAddress)

  val program = List.empty[AddressHistory].iterateUntilM { addresses =>
    println(addresses)
    address(addresses.length, addresses.headOption.map(_.address)).map(_ :: addresses)
  } { _.foldLeft(0)(_ + _.years) >= 5 }

  val result = program.attempt.runSyncUnsafe()

  println(result)
}
