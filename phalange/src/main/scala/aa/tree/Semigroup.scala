package aa.tree

trait Semigroup[F] { self =>
  def append(f1: F, f2: => F): F
}
