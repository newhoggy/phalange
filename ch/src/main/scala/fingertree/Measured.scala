package fingertree

import scalaz.Monoid

trait Measured[V, A] extends Monoid[V] { self =>
  def measure(a: A): V
}
