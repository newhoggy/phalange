package fingertree

import scalaz._
import Implicits._
import Syntax._

case class Vector[+A](tree: FingerTree[Int, A@@Elem]) {
  def size: Int = ToMeasuredOps(tree).measure
  
  def splitAt(i: Int): (Vector[A], Vector[A]) = tree.split(size < _) match {
    case (l, r) => (Vector(l), Vector(r))
  }
  
  def apply(i: Int): A = tree.splitTree(i < _)(0) match {
    case Split(_, x, _) => x
  }
}

object Vector {
  val empty: Vector[Nothing] = new Vector[Nothing](Empty())
  
  def apply[A](values: A*): Vector[A] = new Vector[A](tagF[Elem](values).asTree[Int](MeasuredElemSize[A@@Elem]))
}
