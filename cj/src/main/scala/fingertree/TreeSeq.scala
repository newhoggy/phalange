package fingertree

import scalaz._
import Implicits._
import Syntax._

case class TreeSeq[+A](tree: FingerTree[Int@@Size, A@@Elem]) {
  def size: Int = ToMeasuredOps(tree).measure
  
  def splitAt(i: Int): (TreeSeq[A], TreeSeq[A]) = tree.split(size < _) match {
    case (l, r) => (TreeSeq(l), TreeSeq(r))
  }
  
  def apply(i: Int): A = tree.splitTree(i < _)(Tag[Int, Size](0)) match {
    case Split(_, x, _) => x
  }
}

object TreeSeq {
  val empty: TreeSeq[Nothing] = new TreeSeq[Nothing](Empty())
  
  def apply[A](values: A*): TreeSeq[A] = new TreeSeq[A](values.map(Tag[A, Elem](_)).toList.asTree[Int@@Size](MeasuredElemSize[A@@Elem]))
}
