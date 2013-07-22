import scala.language.higherKinds
import scalaz.@@
import scalaz.Tags._
import fingertree.Syntax._

package object fingertree {
  def !!!(): Nothing = throw new UnsupportedOperationException

  def asList[F[_], A](fa: F[A])(implicit F: Reduce[F]): List[A] = F.reduceR[A, List[A]](_ :: _)(fa, Nil)
  
  def asTree[F[_], V, A](fa: F[A])(implicit F: Reduce[F], M: Measured[V, A]): FingerTree[V, A] = F.reduceR[A, FingerTree[V, A]](_ +: _)(fa, Empty())

  def asList[V, A](digit: Digit[V, A])(implicit F0: Reduce[Digit.α[V]#α]): List[A] = {
    type FV[+A] = FingerTree[V, A]
    type DV[+A] = Digit[V, A]
    type NV[+A] = Node[V, A]

    ToReduceOps[DV, A](digit).asList
  }

  def tag[U] = new Tagger[U]
  
  // Manual specialization needed here ... specializing apply above doesn't help
  def tag[U](i : Int): Int @@ U = i.asInstanceOf[Int @@ U]
  def tag[U](l : Long): Long @@ U = l.asInstanceOf[Long @@ U]
  def tag[U](d : Double): Double @@ U = d.asInstanceOf[Double @@ U]
}
