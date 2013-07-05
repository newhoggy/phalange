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
  final def reverse: LList[A] = foldLeft[LList[A]](Nil)((xs, x) => x +: xs)
  final def foldRight[B](z: B)(f: (A, B) => B): B = reverse.foldLeft(z)((xs, x) => f(x, xs))
}

case object Nil extends LList[Nothing] {
  def head: Nothing = !!!
  def tail: LList[Nothing] = !!!
}

case class Cons[A](head: A, tail: LList[A]) extends LList[A]

object Example {
  import scalaz.Scalaz._

  def main(args: Array[String]): Unit = {
    val left: LList[Int] = 1 +: 2 +: 3 +: Nil
    val right: LList[Int] = 4 +: 5 +: 6 +: Nil
    val both = left |+| right
    println(both)
  }
}
