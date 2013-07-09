package ce.fingertree

trait Sconable[S, A] { self =>
  def scon(sa: S, a: A): S
}

object Sconable {
  def apply[S, A](f: (S, A) => S): Sconable[S, A] = new Sconable[S, A] {
    override def scon(sa: S, a: A): S = f(sa, a)
  }
}
