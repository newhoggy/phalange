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
