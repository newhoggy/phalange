package fingertree

trait Digit[V, +A] {
  def +:[W >: V, B >: A](x: B)(implicit M: Measured[W, B]): Digit[W, B] = this match {
    case D1(v, a         ) => D2(x, a      )
    case D2(v, a, b      ) => D3(x, a, b   )
    case D3(v, a, b, c   ) => D4(x, a, b, c)
    case D4(v, a, b, c, d) => !!!
  }
  def :+[W >: V, B >: A](x: B)(implicit M: Measured[W, B]): Digit[W, B] = this match {
    case D1(v, a         ) => D2(a,       x)
    case D2(v, a, b      ) => D3(a, b,    x)
    case D3(v, a, b, c   ) => D4(a, b, c, x)
    case D4(v, a, b, c, d) => !!!
  }
  def headL: A = this match {
    case D0()              => ???
    case D1(v, a)          => a
    case D2(v, a, _)       => a
    case D3(v, a, _, _)    => a
    case D4(v, a, _, _, _) => a
  }
  def tailL(implicit M: Measured[V, A]): Digit[V, A] = this match {
    case D0()              => ???
    case D1(v, a)          => D0()
    case D2(v, a, b)       => D1(b)
    case D3(v, a, b, c)    => D2(b, c)
    case D4(v, a, b, c, d) => D3(b, c, d)
  }
  def headR: A = this match {
    case D0()              => ???
    case D1(v, a)          => a
    case D2(v, _, b)       => b
    case D3(v, _, _, c)    => c
    case D4(v, _, _, _, d) => d
  }
  def tailR(implicit M: Measured[V, A]): Digit[V, A] = this match {
    case D0()              => ???
    case D1(v, a)          => D0()
    case D2(v, a, b)       => D1(a)
    case D3(v, a, b, c)    => D2(a, b)
    case D4(v, a, b, c, d) => D3(a, b, c)
  }
}

case class D0[V]() extends Digit[V, Nothing]

case class D1[V, +A](v: V, a: A                   ) extends Digit[V, A]

object D1 {
  def apply[A, V](a: A)(implicit M: Measured[V, A]): D1[V, A] = D1(M.measure(a), a)
}

case class D2[V, +A](v: V, a: A, b: A             ) extends Digit[V, A]

object D2 {
  def apply[A, V](a: A, b: A)(implicit M: Measured[V, A]): D2[V, A] = D2(M.measure(a, b), a, b)
}

case class D3[V, +A](v: V, a: A, b: A, c: A       ) extends Digit[V, A]

object D3 {
  def apply[A, V](a: A, b: A, c: A)(implicit M: Measured[V, A]): D3[V, A] = D3(M.measure(a, b, c), a, b, c)
}

case class D4[V, +A](v: V, a: A, b: A, c: A, d: A ) extends Digit[V, A]

object D4 {
  def apply[A, V](a: A, b: A, c: A, d: A)(implicit M: Measured[V, A]): D4[V, A] = D4(M.measure(a, b, c, d), a, b, c, d)
}