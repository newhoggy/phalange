package cg.fingertree

import scalaz.syntax.Ops

trait SconableOps[S, A] extends Ops[S] {
  implicit def F: Sconable[S, A]

  def :+(a: A) : S = F.scon(self, a)
}

object Sonable {
  def apply[S, A](f: (S, A) => S): Sconable[S, A] = new Sconable[S, A] {
    override def scon(sa: S, a: A): S = f(sa, a)
  }
}
