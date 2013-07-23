package fingertree

import scalaz._
import Syntax._

trait VectorImplicits extends Implicits {
  implicit object MonoidSize extends Monoid[Int] {
    override def zero: Int = 0
    override def append(a: Int, b: => Int) = a + b
  }
  
  implicit def MeasuredElemSize[V] = new Measured[Int, V@@Elem] {
    override implicit def monoid: Monoid[Int] = MonoidSize

    override def measure(tree: V@@Elem): Int = 1
  }
}

case class Vector[+A](tree: FingerTree[Int, A@@Elem]) {
  import Vector._
  
  def size: Int = ToMeasuredOps(tree).measure
  
  def splitAt(i: Int): (Vector[A], Vector[A]) = tree.split(size < _) match {
    case (l, r) => (Vector(l), Vector(r))
  }
  
  def apply(i: Int): A = tree.splitTree(i < _)(0) match {
    case Split(_, x, _) => x
  }
}

object Vector extends VectorImplicits {
  val empty: Vector[Nothing] = new Vector[Nothing](Empty())
  
  def apply[A](values: A*): Vector[A] = new Vector[A](tagF[Elem](values).asTree[Int](MeasuredElemSize[A@@Elem]))
}
