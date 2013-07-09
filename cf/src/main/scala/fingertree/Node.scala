package cf.fingertree

trait Node[+A] {
  def toDigit: Digit[A] = this match {
    case N2(a, b   ) => D2(a, b)
    case N3(a, b, c) => D3(a, b, c)
  }
}

case class N2[A](a: A, b: A       ) extends Node[A]
case class N3[A](a: A, b: A, c: A ) extends Node[A]
