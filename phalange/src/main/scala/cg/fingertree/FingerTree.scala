package cg.fingertree

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
  
  def viewL: ViewL[FingerTree, A] = this match {
    case Empty => EmptyL
    case Single(x) => ConsL(x, Empty)
    case Deep(l, m, r) => ConsL(l.headL, FingerTree.deepL(l.tailL, m, r))
  }
  
  def viewR: ViewR[FingerTree, A] = this match {
    case Empty => EmptyR
    case Single(x) => ConsR(Empty, x)
    case Deep(l, m, r) => ConsR(FingerTree.deepL(l, m, r.tailR), r.headR)
  }
}

case object Empty extends FingerTree[Nothing]

case class Single[+A](a: A) extends FingerTree[A]

case class Deep[A](l: Digit[A], m: FingerTree[Node[A]], r: Digit[A]) extends FingerTree[A]

object FingerTree {
  import Implicits._
  import Syntax._
  def deepL[A](l: Digit[A], m: FingerTree[Node[A]], r: Digit[A]): FingerTree[A] = l match {
    case D0 => m.viewL match {
      case EmptyL => r.toTree
      case ConsL(ma, mm) => Deep(ma.toDigit, mm, r) 
    }
    case _ => Deep(l, m, r)
  }
}
