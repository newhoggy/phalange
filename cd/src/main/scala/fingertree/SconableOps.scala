package fingertree

import scalaz.syntax.Ops

trait SconableOps[S, A] extends Ops[S] {
  implicit def F: Sconable[S, A]

  def :+(a: A) : S = F.scon(self, a)
}
