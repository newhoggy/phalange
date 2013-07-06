package cc.fingertree

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

trait FingerTree[+A] {
  def +:[B >: A](x: B): FingerTree[B] = this match {
    case Empty                      => Single(x)
    case Single(v)                  => Deep(D1(x)   , Empty           , D1(v) )
    case Deep(D4(a, b, c, d), m, r) => Deep(D2(a, b), N3(c, d, x) +: m, r     )
    case Deep(l             , m, r) => Deep(x +: l  , m               , r     )
  }

  def :+[B >: A](x: B): FingerTree[B] = this match {
    case Empty                      => Single(x)
    case Single(v)                  => Deep(D1(v) , Empty           , D1(x)   )
    case Deep(l, m, D4(a, b, c, d)) => Deep(l     , m :+ N3(a, b, c), D2(d, x))
    case Deep(l, m, r             ) => Deep(l     , m               , r :+ x  )
  }
//  
//  final def foldLeft[B](xs: B)(f: (B, A) => B): B = this match {
//    case Empty => xs
//    case Single(v) => f(xs, v)
//    case Deep(l, m, r) => 
//  }
}

case object Empty extends FingerTree[Nothing]
case class Single[+A](a: A) extends FingerTree[A]
case class Deep[A](l: Digit[A], m: FingerTree[Node[A]], r: Digit[A]) extends FingerTree[A]

trait Node[+A]
case class N2[A](a: A, b: A       ) extends Node[A]
case class N3[A](a: A, b: A, c: A ) extends Node[A]
