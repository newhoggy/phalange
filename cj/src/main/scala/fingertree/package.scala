package object fingertree {
  import scalaz.@@
  import scalaz.Tags._
  import Syntax._

  def !!!(): Nothing = throw new UnsupportedOperationException

  def asList[V, A](digit: Digit[V, A])(implicit F0: Reduce[Digit.α[V]#α]): List[A] = {
    type FV[+A] = FingerTree[V, A]
    type DV[+A] = Digit[V, A]
    type NV[+A] = Node[V, A]

    ToReduceOps[DV, A](digit).asList
  }

  def tag[U] = new Tagger[U]
  
  // Manual specialization needed here ... specializing apply above doesn't help
  def tag[U](i : Int) : Int @@ U = i.asInstanceOf[Int @@ U]
  def tag[U](l : Long) : Long @@ U = l.asInstanceOf[Long @@ U]
  def tag[U](d : Double) : Double @@ U = d.asInstanceOf[Double @@ U]
}
