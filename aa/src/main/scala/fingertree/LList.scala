package fingertree

trait LList[+A] {
  def head: A
  def tail: LList[A]
  def +:[B >: A](item: B): LList[B] = Cons(item, this)
}

case object Nil extends LList[Nothing] {
  def head: Nothing = !!!
  def tail: LList[Nothing] = !!!
}

case class Cons[A](head: A, tail: LList[A]) extends LList[A]

object Example {
  def main(args: Array[String]): Unit = {
    val empty: LList[Int] = Nil
    val single: LList[Int] = 1 +: 1 +: Nil
  }
}
