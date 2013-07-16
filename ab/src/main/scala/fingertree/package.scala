import scalaz.Monoid

package object fingertree {
  def !!!(): Nothing = throw new UnsupportedOperationException
  
  implicit def LListMonoid[F]: Monoid[List[F]] = new Monoid[List[F]] {
    override def zero: List[F] = Nil
    override def append(a: List[F], b: => List[F]): List[F] = a.foldRight(b)(_ +: _)
  }
}
