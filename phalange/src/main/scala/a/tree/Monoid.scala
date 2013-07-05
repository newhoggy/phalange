package a.tree

trait Monoid[F] extends Semigroup[F] { self =>
  def zero: F
}
