package fingertree

import scala.annotation.tailrec

trait List[+A] {
  def +:[B >: A](item: B): List[B] = Cons(item, this)
  @tailrec
  final def foldLeft[B](b: B)(f: (A, B) => B): B = this match {
    case Nil => b
    case Cons(head, tail) => tail.foldLeft(f(head, b))(f)
  }
}

case object Nil extends List[Nothing]

case class Cons[A](head: A, tail: List[A]) extends List[A]

object Example {
  def main(args: Array[String]): Unit = {
    val empty: List[Int] = Nil
    val list: List[Int] = 1 +: 2 +: Nil
    val sum = list.foldLeft(0)(_ + _)
    val revered = list.foldLeft[List[Int]](Nil)(_ +: _)
  }
}
