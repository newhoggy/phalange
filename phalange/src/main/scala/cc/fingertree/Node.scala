package cc.fingertree

trait Node[+A] {
  def toDigit: Digit[A] = ???
}

case class N2[A](a: A, b: A       ) extends Node[A]
case class N3[A](a: A, b: A, c: A ) extends Node[A]