package cg.fingertree

import scalaz.syntax.Ops

trait ConsableOps[A, S] extends Ops[S] {
  implicit def F: Consable[A, S]

  def +:(a: A) : S = F.cons(a, self)

  def ++:(a: A) : S = F.cons(a, self)
}
