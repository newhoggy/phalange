package fingertree

import scalaz.Monoid
import scalaz.Scalaz._

trait Measured[V, -A] { self =>
  implicit def monoid: Monoid[V]
  
  def measure(a: A): V
  
  def measure(a: A, b: A): V = self.measure(a) |+| self.measure(b)
  
  def measure(a: A, b: A, c: A): V = self.measure(a) |+| self.measure(b) |+| self.measure(c)
  
  def measure(a: A, b: A, c: A, d: A): V = self.measure(a) |+| self.measure(b) |+| self.measure(c) |+| self.measure(d)
}
