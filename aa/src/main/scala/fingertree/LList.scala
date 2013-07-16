package fingertree

import scala.annotation.tailrec

trait List[+A] {
  def head: A
  def tail: List[A]
  def +:[B >: A](item: B): List[B] = Cons(item, this)
  @tailrec
  final def foldLeft[B](f: (A, => B) => B)(b: B): B = this match {
    case Nil => b
    case Cons(head, tail) => tail.foldLeft(f)(f(head, b))
  }
}

case object Nil extends List[Nothing] {
  def head: Nothing = !!!
  def tail: List[Nothing] = !!!
}

case class Cons[A](head: A, tail: List[A]) extends List[A]

object Example {
  def main(args: Array[String]): Unit = {
    val empty: List[Int] = Nil
    val single: List[Int] = 1 +: 2 +: Nil
  }
}
