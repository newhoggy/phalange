package cd.fingertree

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
  def headL: A = this match {
    case D0             => ???
    case D1(a)          => a
    case D2(a, _)       => a
    case D3(a, _, _)    => a
    case D4(a, _, _, _) => a
  }
  def tailL: Digit[A] = this match {
    case D0             => ???
    case D1(a)          => D0
    case D2(a, b)       => D1(b)
    case D3(a, b, c)    => D2(b, c)
    case D4(a, b, c, d) => D3(b, c, d)
  }
  def headR: A = this match {
    case D0             => ???
    case D1(a)          => a
    case D2(_, b)       => b
    case D3(_, _, c)    => c
    case D4(_, _, _, d) => d
  }
  def tailR: Digit[A] = this match {
    case D0             => ???
    case D1(a)          => D0
    case D2(a, b)       => D1(a)
    case D3(a, b, c)    => D2(a, b)
    case D4(a, b, c, d) => D3(a, b, c)
  }
  def toTree: FingerTree[A] = ???
}

case object D0 extends Digit[Nothing]
case class D1[A](a: A                   ) extends Digit[A]
case class D2[A](a: A, b: A             ) extends Digit[A]
case class D3[A](a: A, b: A, c: A       ) extends Digit[A]
case class D4[A](a: A, b: A, c: A, d: A ) extends Digit[A]
