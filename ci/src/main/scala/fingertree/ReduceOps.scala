package fingertree

import scala.language.higherKinds
import scalaz.syntax.Ops

trait ReduceOps[F[_], A] extends Ops[F[A]] {
  implicit def F: Reduce[F]

  def asList: List[A] = F.reduceR[A, List[A]](_ :: _)(self, Nil)
  
  def asTree[V](implicit M: Measured[V, A]): FingerTree[V, A] = F.reduceR[A, FingerTree[V, A]](_ +: _)(self, Empty())
}
