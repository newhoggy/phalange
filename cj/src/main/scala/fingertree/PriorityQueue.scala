package fingertree

import scalaz.Monoid
import scalaz.Order
import scalaz.Ordering
import scalaz.@@
import scalaz.Scalaz._
import Syntax._

trait Priority[+A]
case object MinusInfinity extends Priority[Nothing]
case class SomePriority[+A](value: A) extends Priority[A] 

trait PriorityQueueImplicits extends Implicits {
  implicit def PriorityOrder[A: Order](): Order[Priority[A]] = new Order[Priority[A]] {
    override def order(l: Priority[A], r: Priority[A]): Ordering = (l, r) match {
      case (MinusInfinity, MinusInfinity)       => Ordering.EQ
      case (_, MinusInfinity)                   => Ordering.GT
      case (MinusInfinity, _r)                  => Ordering.LT
      case (SomePriority(lp), SomePriority(rp)) => Order[A].order(lp, rp)
    }
  }
  
  implicit def PriorityMonoid[A: Order](): Monoid[Priority[A]] = new Monoid[Priority[A]] {
    override def zero: Priority[A] = MinusInfinity
    override def append(l: Priority[A], r: => Priority[A]): Priority[A] = (l, r) match {
      case (l, MinusInfinity) => l
      case (MinusInfinity, r) => r
      case (SomePriority(lp), SomePriority(rp)) => SomePriority(lp max rp)
    }
  }

  implicit def MeasuredElemPriority[A: Order] = new Measured[Priority[A], A@@Elem] {
    override implicit def monoid: Monoid[Priority[A]] = PriorityMonoid[A]

    override def measure(elem: A@@Elem): Priority[A] = SomePriority(elem)
  }
//  
//  implicit def MeasuredElemPriorityFingerTree[A: Order] = new Measured[A, FingerTree[Priority[A], A@@Elem]] {
//    override implicit def monoid: Monoid[Priority[A]] = PriorityMonoid[A]
//
//    override def measure(elem: A): Priority[A] = SomePriority(elem)
//  }
}

case class PriorityQueue[A](tree: FingerTree[Priority[A], A@@Elem]) {
  import PriorityQueue._

  implicit private def order(implicit O: Order[A]) = PriorityOrder[A]

  def extractMax(implicit O: Order[A]): (A, PriorityQueue[A]) = {
    val m =  ToMeasuredOps(tree).measure
    tree.splitTree(m <= _)(MinusInfinity) match {
      case Split(l, x, r) => (x, PriorityQueue(l ++ r))
    }
  }
}

object PriorityQueue extends PriorityQueueImplicits
