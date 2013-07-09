package cf.fingertree

trait ViewL[S[_], +A] {
//  def fold[B](b: => B, f: (=> A, => S[A]) => B): B
//  def headOption: Option[A] = fold(None, (a, sa) => Some(a))
//  def tailOption: Option[S[A]] = fold(None, (a, sa) => Some(sa))
//  def head: A = headOption.getOrElse(sys.error("Head on empty view"))
//  def tail: S[A] = tailOption.getOrElse(sys.error("Tail on empty view"))
}

case class EmptyL[S[_], A]() extends ViewL[S, A]

case class ConsL[S[_], A](a: A, sa: S[A]) extends ViewL[S, A]
