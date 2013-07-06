package cd.fingertree

import scalaz.syntax.Ops

trait ReduceOps[F[_], A] extends Ops[F[A]] {
  implicit def F: Reduce[F]
  
  def toList: List[A] = F.reduceR[A, List[A]]((x, xs) => x::xs)(Nil)(self)
}
