package fingertree

import scalaz.syntax.Ops

trait SnocableOps[S, A] extends Ops[S] {
  implicit def F: Snocable[S, A]

  def :+(a: A) : S = F.snoc(self, a)

  def :++(a: A) : S = F.snoc(self, a)
}
