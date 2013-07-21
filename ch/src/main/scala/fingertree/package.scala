import scala.language.higherKinds

package object fingertree {
  def !!!(): Nothing = throw new UnsupportedOperationException

  def asList[F[_], A](fa: F[A])(implicit F: Reduce[F]): List[A] = F.reduceR[A, List[A]](_ :: _)(fa, Nil)
  
  def asTree[F[_], V, A](fa: F[A])(implicit F: Reduce[F], M: Measured[V, A]): FingerTree[V, A] = F.reduceR[A, FingerTree[V, A]](_ +: _)(fa, Empty())
}
