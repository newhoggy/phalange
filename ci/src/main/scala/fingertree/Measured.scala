package fingertree

import scalaz.Monoid
import scalaz.Scalaz._

trait Measured[V, -A] {
  implicit def monoid: Monoid[V]
  
  def measure(a: A): V
  
  def measure(a: A, b: A): V = measure(a) |+| measure(b)
  
  def measure(a: A, b: A, c: A): V = measure(a) |+| measure(b) |+| measure(c)
  
  def measure(a: A, b: A, c: A, d: A): V = measure(a) |+| measure(b) |+| measure(c) |+| measure(d)
}
