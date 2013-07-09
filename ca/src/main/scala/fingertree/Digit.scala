package fingertree

trait Digit[+A]
case class D1[A](a: A                   ) extends Digit[A]
case class D2[A](a: A, b: A             ) extends Digit[A]
case class D3[A](a: A, b: A, c: A       ) extends Digit[A]
case class D4[A](a: A, b: A, c: A, d: A ) extends Digit[A]
