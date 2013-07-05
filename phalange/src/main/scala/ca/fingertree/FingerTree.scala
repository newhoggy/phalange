package ca.fingertree

trait Digit[+A]
case class D1[A](a: A                   ) extends Digit[A]
case class D2[A](a: A, b: A             ) extends Digit[A]
case class D3[A](a: A, b: A, c: A       ) extends Digit[A]
case class D4[A](a: A, b: A, c: A, d: A ) extends Digit[A]

trait FingerTree[+A]
case object Empty extends FingerTree[Nothing]
case class Single[+A](a: A) extends FingerTree[A]
case class Deep[A](l: Digit[A], m: FingerTree[Node[A]], r: Digit[A]) extends FingerTree[A]

trait Node[+A]
case class N2[A](a: A, b: A       ) extends Node[A]
case class N3[A](a: A, b: A, c: A ) extends Node[A]

object Data {
  val x: FingerTree[Char] = Deep(D2('t', 'h'), Empty, D3('r', 'e', 'e'))
  val y: FingerTree[Char] = {
      Deep(
          D2('t', 'h'),
          Deep(
              D2(N2('i', 's'), N2('i', 's')),
              Empty,
              D2(N3('n', 'o', 't'), N2('a', 't'))),
          D3('r', 'e', 'e'))
  }
}
