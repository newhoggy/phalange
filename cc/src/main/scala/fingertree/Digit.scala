package fingertree

trait Digit[+A] {
  def +:[B >: A](x: B): Digit[B] = this match {
    case D1(a         ) => D2(x, a      )
    case D2(a, b      ) => D3(x, a, b   )
    case D3(a, b, c   ) => D4(x, a, b, c)
    case D4(a, b, c, d) => !!!
  }
  def :+[B >: A](x: B): Digit[B] = this match {
    case D1(a         ) => D2(a,       x)
    case D2(a, b      ) => D3(a, b,    x)
    case D3(a, b, c   ) => D4(a, b, c, x)
    case D4(a, b, c, d) => !!!
  }
}

case class D1[A](a: A                   ) extends Digit[A]
case class D2[A](a: A, b: A             ) extends Digit[A]
case class D3[A](a: A, b: A, c: A       ) extends Digit[A]
case class D4[A](a: A, b: A, c: A, d: A ) extends Digit[A]
