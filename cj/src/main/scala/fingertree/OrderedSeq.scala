package fingertree

import scalaz.Monoid
import scalaz.Order
import scalaz.@@

trait Key[+K]
case object NoKey extends Key[Nothing]
case class SomeKey[K](key: K) extends Key[K]

trait OrderedSeqImplicits extends Implicits {
  implicit def KeyMonoid[A: Order](): Monoid[Key[A]] = new Monoid[Key[A]] {
    override def zero: Key[A] = NoKey
    override def append(l: Key[A], r: => Key[A]): Key[A] = (l, r) match {
      case (l, NoKey) => l
      case (l, r) => r
    }
  }
}

case class OrderedSeq[K, +A <: K](tree: FingerTree[Key[K], A@@Elem]) {
  def +:[B >: A <: K](a: B): OrderedSeq[K, B] = OrderedSeq(tree)
}

object OrderedSeq extends OrderedSeqImplicits
