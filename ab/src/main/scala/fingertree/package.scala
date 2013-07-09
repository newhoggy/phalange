import scalaz.Monoid

package object fingertree {
  def !!!(): Nothing = throw new UnsupportedOperationException
  
  implicit def LListMonoid[F]: Monoid[LList[F]] = new Monoid[LList[F]] {
    override def zero: LList[F] = Nil
    override def append(a: LList[F], b: => LList[F]): LList[F] = a.foldRight(b)(_ +: _)
  }
}
