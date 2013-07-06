package cc.fingertree

trait FingerTree[+A] {
  def +:[B >: A](x: B): FingerTree[B] = this match {
    case Empty                      => Single(x)
    case Single(v)                  => Deep(D1(x)   , Empty           , D1(v) )
    case Deep(D4(a, b, c, d), m, r) => Deep(D2(x, a), N3(b, c, d) +: m, r     )
    case Deep(l             , m, r) => Deep(x +: l  , m               , r     )
  }

  def :+[B >: A](x: B): FingerTree[B] = this match {
    case Empty                      => Single(x)
    case Single(v)                  => Deep(D1(v) , Empty           , D1(x)   )
    case Deep(l, m, D4(a, b, c, d)) => Deep(l     , m :+ N3(a, b, c), D2(d, x))
    case Deep(l, m, r             ) => Deep(l     , m               , r :+ x  )
  }
}

case object Empty extends FingerTree[Nothing]
case class Single[+A](a: A) extends FingerTree[A]
case class Deep[A](l: Digit[A], m: FingerTree[Node[A]], r: Digit[A]) extends FingerTree[A]
