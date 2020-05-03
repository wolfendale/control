package wolfendale

import cats.implicits._

object IterApplication extends App {

  val initialHistory = Iter.continually("")
  val exampleHistory = "^D" ::: "^C" ::: "eat flaming death" ::: "hello?" ::: "bye" ::: initialHistory

  println(exampleHistory.next.next.extract)

  println(exampleHistory.extend(_.next.extract))
  println(exampleHistory.next)

  println(exampleHistory.extend(_.extract))
  println(exampleHistory)

  val l1  = List(1, 2, 3)
  val i1  = Iter.fromListM(l1)
  val i1p = l1.foldLeft(i1) { (m, n) =>
    println(s"$n == ${m.extract}")
    m.next
  }
  println("consumed list, should be Monoid[A].empty")
  println(i1p.extract)

  val l2  = List(1, 2, 3)
  val i2  = Iter.fromList(l1)
  val i2p = l2.foldLeft(i2) { (m, n) =>
    println(s"Some($n) == ${m.extract}")
    m.next
  }
  println("consumed list, should be None")
  println(i2p.extract)
}
