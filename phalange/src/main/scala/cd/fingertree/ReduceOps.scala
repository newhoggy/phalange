package cd.fingertree

import scalaz.syntax.Ops

trait ReduceOps[F[_], A] extends Ops[F[A]] {
  implicit def F: Reduce[F]
  
  // Note: Renamed from toList to asList to avoid conflict with built-in Scala toList method
  def asList: List[A] = F.reduceR[A, List[A]](_::_)(Nil)(self)
  
  def toTree: FingerTree[A] = F.reduceR[A, FingerTree[A]](_+:_)(Empty)(self)
}
