package fingertree

import scalaz.syntax.Ops

trait ReduceOps[F[_], A] extends Ops[F[A]] {
  implicit def F: Reduce[F]

  // Note: Renamed from toList to asList to avoid conflict with built-in Scala toList method
  def asList: List[A] = F.reduceR[A, List[A]](_::_)(self)(Nil)
  
  def toTree[V](implicit M: Measured[V, A]): FingerTree[V, A] = F.reduceR[A, FingerTree[V, A]](_+:_)(self)(Empty())
}
