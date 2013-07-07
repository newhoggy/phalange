package cc.fingertree

trait ViewR[S[_], A] {
//  def fold[B](b: => B, f: (=> S[A], => A) => B): B
//  def lastOption: Option[A] = fold(None, (sa, a) => Some(a))
//  def initOption: Option[S[A]] = fold(None, (sa, a) => Some(sa))
//  def last: A = lastOption.getOrElse(sys.error("Last on empty view"))
//  def init: S[A] = initOption.getOrElse(sys.error("Init on empty view"))
}

case object EmptyR extends ViewR[Nothing, Nothing]

case class ConsR[S[_], A](a: A, sa: S[A])
