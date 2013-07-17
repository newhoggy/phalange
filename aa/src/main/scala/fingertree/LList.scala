package fingertree

import scala.annotation.tailrec

trait List[+A] {
  def +:[B >: A](item: B): List[B] = Cons(item, this)
}

case object Nil extends List[Nothing]

case class Cons[A](head: A, tail: List[A]) extends List[A]

object Example {
  def main(args: Array[String]): Unit = {
    val empty: List[Int] = Nil
    val list: List[Int] = 1 +: 2 +: Nil
  }
}
