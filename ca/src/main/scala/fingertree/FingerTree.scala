package fingertree

trait FingerTree[+A]

case object Empty extends FingerTree[Nothing]

case class Single[+A](a: A) extends FingerTree[A]

case class Deep[A](l: Digit[A], m: FingerTree[Node[A]], r: Digit[A]) extends FingerTree[A]
