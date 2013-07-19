package fingertree

trait Snocable[S, A] {
  def snoc(sa: S, a: A): S
}

object Snocable {
  def apply[S, A](f: (S, A) => S): Snocable[S, A] = new Snocable[S, A] {
    override def snoc(sa: S, a: A): S = f(sa, a)
  }
}
