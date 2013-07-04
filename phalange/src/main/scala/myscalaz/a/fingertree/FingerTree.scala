package myscalaz.a.fingertree

trait Digit[+A]
case class D1[A](a: A) extends Digit[A]
case class D2[A](a: A, b: A) extends Digit[A]
case class D3[A](a: A, b: A, c: A) extends Digit[A]
case class D4[A](a: A, b: A, c: A, d: A) extends Digit[A]

trait FingerTree[+A]
case object Empty extends FingerTree[Nothing]
case class Single[+A](a: A) extends FingerTree[A]
case class Deep[A](l: Digit[A], m: FingerTree[Node[A]], r: Digit[A]) extends FingerTree[A]

trait Node[+A]
case class Node2[A](a: A, b: A) extends Node[A]
case class Node3[A](a: A, b: A, c: A) extends Node[A]

object Data {
  val x: FingerTree[Char] = Deep(D2('t', 'h'), Empty, D3('r', 'e', 'e'))
  val y: FingerTree[Char] = {
      Deep(
          D2('t', 'h'),
          Deep(
              D2(Node2('i', 's'), Node2('i', 's')),
              Empty,
              D2(Node3('n', 'o', 't'), Node2('a', 't'))),
          D3('r', 'e', 'e'))
  }
}
