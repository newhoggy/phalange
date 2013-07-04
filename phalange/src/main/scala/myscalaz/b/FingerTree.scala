package myscalaz.b

trait Digit[+A]
case class One[A](a: A) extends Digit[A]
case class Two[A](a: A, b: A) extends Digit[A]
case class Three[A](a: A, b: A, c: A) extends Digit[A]
case class Four[A](a: A, b: A, c: A, d: A) extends Digit[A]

trait FingerTree[+A]
case object Empty extends FingerTree[Nothing]
case class Single[+A](a: A) extends FingerTree[A]
case class Deep[A](l: Digit[A], m: FingerTree[Node[A]], r: Digit[A]) extends FingerTree[A]

trait Node[+A]
case class Node2[A](a: A, b: A) extends Node[A]
case class Node3[A](a: A, b: A, c: A) extends Node[A]

object Data {
  val x: FingerTree[Char] = Deep(Two('t', 'h'), Empty, Three('r', 'e', 'e'))
  val y: FingerTree[Char] = {
      Deep(
          Two('t', 'h'),
          Deep(
              Two(Node2('i', 's'), Node2('i', 's')),
              Empty,
              Two(Node3('n', 'o', 't'), Node2('a', 't'))),
          Three('r', 'e', 'e'))
  }
}
