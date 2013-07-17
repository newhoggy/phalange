package fingertree

import scala.annotation.tailrec
import scalaz.Scalaz.ToSemigroupOps

trait List[+A] {
  def +:[B >: A](item: B): List[B] = Cons(item, this)
  @tailrec
  final def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
    case Cons(head, tail) => tail.foldLeft(f(z, head))(f)
    case Nil => z
  }
  final def reverse: List[A] = foldLeft[List[A]](Nil)((xs, x) => x +: xs)
  final def foldRight[B](z: B)(f: (A, B) => B): B = reverse.foldLeft(z)((xs, x) => f(x, xs))
}

case object Nil extends List[Nothing]

case class Cons[A](head: A, tail: List[A]) extends List[A]

object Example {
  import scalaz.Scalaz._

  def main(args: Array[String]): Unit = {
    val left: List[Int] = 1 +: 2 +: 3 +: Nil
    val right: List[Int] = 4 +: 5 +: 6 +: Nil
    val sumLeft = left.foldLeft(0)(_ + _)
    val both = left |+| right
    println(both)
  }
}
