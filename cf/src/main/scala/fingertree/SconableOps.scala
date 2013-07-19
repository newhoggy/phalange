package fingertree

import scalaz.syntax.Ops

trait SnocableOps[S, A] extends Ops[S] {
  implicit def F: Snocable[S, A]

  def :+(a: A) : S = F.snoc(self, a)
}

object Sonable {
  def apply[S, A](f: (S, A) => S): Snocable[S, A] = new Snocable[S, A] {
    override def snoc(sa: S, a: A): S = f(sa, a)
  }
}
