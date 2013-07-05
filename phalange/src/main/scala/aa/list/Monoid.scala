package aa.list

trait Monoid[F] extends Semigroup[F] { self =>
  def zero: F
}
