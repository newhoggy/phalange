import scala.language.higherKinds

package object fingertree {
  def !!!(): Nothing = throw new UnsupportedOperationException

  def asList[F[_], A](fa: F[A])(implicit F: Reduce[F]): List[A] = F.reduceR[A, List[A]](_ :: _)(fa, Nil)
  
  def asTree[F[_], A](fa: F[A])(implicit F: Reduce[F]): FingerTree[A] = F.reduceR[A, FingerTree[A]](_ +: _)(fa, Empty)
}
