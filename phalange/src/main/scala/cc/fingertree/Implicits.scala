package cc.fingertree

object Implicits {
  implicit object ReduceFingerTree extends Reduce[FingerTree] {
    override def reduceR[A, B](f: (A, => B) => B)(z: => B)(fa: FingerTree[A]): B = fa match {
      case Empty => z
      case Single(a) => f(a, z)
      case Deep(l, m, r) => ???
    }
    override def reduceL[A, B](f: (B,    A) => B)(z:    B)(fa: FingerTree[A]): B = ???
  }

  implicit object ReduceDigit extends Reduce[Digit] {
    override def reduceR[A, B](f: (A, => B) => B)(z: => B)(fa: Digit[A]): B = fa match {
      case D1(a         ) =>           f(a, z)
      case D2(a, b      ) =>      f(a, f(b, z))
      case D3(a, b, c   ) => f(a, f(b, f(c, z)))
      case D4(a, b, c, d) => !!!
    }
    override def reduceL[A, B](f: (B,    A) => B)(z:    B)(fa: Digit[A]): B = fa match {
      case D1(a         ) =>     f(z, a)
      case D2(a, b      ) =>   f(f(z, a), b)
      case D3(a, b, c   ) => f(f(f(z, a), b), c)
      case D4(a, b, c, d) => !!!
    }
  }
}
