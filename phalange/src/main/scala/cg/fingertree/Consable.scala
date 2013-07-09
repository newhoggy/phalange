package cg.fingertree

trait Consable[A, S] { self =>
  def cons(a: A, sa: S): S
}

object Consable {
  def apply[A, S](f: (A, => S) => S): Consable[A, S] = new Consable[A, S] {
    override def cons(a: A, sa: S): S = f(a, sa)
  }
}
