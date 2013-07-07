package cc.fingertree

import scalaz.syntax.Ops

trait ReduceOps[F[_], A] extends Ops[F[A]] {
  implicit def F: Reduce[F]
  
  def asList: List[A] = F.reduceR[A, List[A]](_::_)(Nil)(self)
}
