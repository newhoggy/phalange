package fingertree

import scala.language.higherKinds

trait ViewL[+S[+_], +A] {
  def headOption: Option[A] = this match {
    case EmptyL => None
    case ConsL(a, sa) => Some(a)
  }

  def tailOption: Option[S[A]] = this match {
    case EmptyL => None
    case ConsL(a, sa) => Some(sa)
  }

  def lHead: A = headOption.getOrElse(sys.error("Head on empty view"))

  def lTail: S[A] = tailOption.getOrElse(sys.error("Tail on empty view"))
}

case object EmptyL extends ViewL[Nothing, Nothing]

case class ConsL[+S[+_], +A](a: A, sa: S[A]) extends ViewL[S, A]
