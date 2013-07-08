package ce.fingertree

import scalaz.syntax.Ops

trait ReduceOps[F[_], A] extends Ops[F[A]] {
  implicit def F: Reduce[F]

//  def +::(a: A, b: A): Unit = F.reduce2R
  
  // Note: Renamed from toList to asList to avoid conflict with built-in Scala toList method
  def asList: List[A] = F.reduceR[A, List[A]](_::_)(self)(Nil)
  
  def toTree: FingerTree[A] = F.reduceR[A, FingerTree[A]](_+:_)(self)(Empty)
}
