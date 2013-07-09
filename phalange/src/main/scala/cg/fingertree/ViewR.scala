package cg.fingertree

trait ViewR[+S[+_], +A] {
  def headOption: Option[A] = this match {
    case EmptyR => None
    case ConsR(sa, a) => Some(a)
  }

  def tailOption: Option[S[A]] = this match {
    case EmptyR => None
    case ConsR(sa, a) => Some(sa)
  }

  def headr: A = headOption.getOrElse(sys.error("Head on empty view"))

  def tailr: S[A] = tailOption.getOrElse(sys.error("Tail on empty view"))
}

case object EmptyR extends ViewR[Nothing, Nothing]

case class ConsR[S[+_], +A](sa: S[A], a: A) extends ViewR[S, A]
