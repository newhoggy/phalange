package cc.fingertree

object Implicits {
  implicit object ReduceFingerTree extends Reduce[FingerTree] {
    override def reduceR[A, B](f: (A, => B) => B)(z: => B)(fa: FingerTree[A]): B = ???
    override def reduceL[A, B](f: (B,    A) => B)(z:    B)(fa: FingerTree[A]): B = ???
  }
}
