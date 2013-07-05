package ab.list

import scala.annotation.tailrec

trait LList[+A] {
  def head: A
  def tail: LList[A]
  def +:[B >: A](item: B): LList[B] = Cons(item, this)
  @tailrec
  final def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
    case Cons(head, tail) => tail.foldLeft(f(z, head))(f)
    case Nil => z
  }
}

case object Nil extends LList[Nothing] {
  def head: Nothing = !!!
  def tail: LList[Nothing] = !!!
}

case class Cons[A](head: A, tail: LList[A]) extends LList[A]

object LListData {
  def main(args: Array[String]): Unit = {
    val empty: LList[Int] = Nil
    val single: LList[Int] = 1 +: Nil
    List().foldLeft(???)(???)
  }
}
