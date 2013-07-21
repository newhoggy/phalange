package fingertree

import scalaz.Scalaz, Scalaz._
import scalaz.Monoid

trait FingerTree[V, +A] {
  type FV[+A] = FingerTree[V, A]
  type DV[+A] = Digit[V, A]
  
  import Implicits._
  
  def +:[B >: A](x: B)(implicit M: Measured[V, B]): FingerTree[V, B] = (this: FingerTree[V, B]) match {
    case Empty()                          => Single(M.measure(x), x)
    case Single(v, y)                     => Deep(D1(x   ), Empty()         , D1(y))
    case Deep(_, D4(_, a, b, c, d), m, r) => Deep(D2(x, a), N3(b, c, d) +: m, r    )
    case Deep(_, l                , m, r) => Deep(x +: l  , m               , r    )
  }

  def :+[B >: A](x: B)(implicit M: Measured[V, B]): FingerTree[V, B] = (this: FingerTree[V, B]) match {
    case Empty()                          => Single(M.measure(x), x)
    case Single(_, x)                     => Deep(D1(x), Empty()         , D1(x   ))
    case Deep(_, l, m, D4(v, a, b, c, d)) => Deep(l    , m :+ N3(a, b, c), D2(d, x))
    case Deep(_, l, m, r                ) => Deep(l    , m               , r :+ x  )
  }
  
  def ++[B >: A](that: FingerTree[V, B])(implicit M: Measured[V, B]): FingerTree[V, B] = FingerTree.append3[V, B](this, Nil, that)
  
  def viewL(implicit M: Measured[V, A]): ViewL[({type X[+A]=FingerTree[V, A]})#X, A] = {
    type FV[+A] = FingerTree[V, A]
    this match {
      case Empty()          => EmptyL
      case Single(v, x)     => ConsL[FV, A](x, Empty())
      case Deep(_, l, m, r) => ConsL[FV, A](l.headL, FingerTree.deepL(l.tailL, m, r))
    }
  }
  
  def viewR(implicit M: Measured[V, A]): ViewR[({type X[+A]=FingerTree[V, A]})#X, A] = {
    type FV[+A] = FingerTree[V, A]
    this match {
      case Empty()          => EmptyR
      case Single(v, x)     => ConsR[FV, A](Empty(), x)
      case Deep(_, l, m, r) => ConsR[FV, A](FingerTree.deepL(l, m, r.tailR), r.headR)
    }
  }
}

case class Empty[V]() extends FingerTree[V, Nothing]

case class Single[V, +A](v: V, a: A) extends FingerTree[V, A]

case class Deep[V, A](v: V, l: Digit[V, A], m: FingerTree[V, Node[V, A]], r: Digit[V, A]) extends FingerTree[V, A]

object Deep {
  import Implicits._
  import Syntax._
  
  def apply[V, A](l: Digit[V, A], m: FingerTree[V, Node[V, A]], r: Digit[V, A])(implicit M: Measured[V, A]): Deep[V, A] = {
    import M.monoid
    Deep(ToMeasuredOps(l).measure |+| ToMeasuredOps(m).measure |+| ToMeasuredOps(r).measure, l, m, r)
  }
}

object FingerTree {
  import Implicits._
  import Syntax._

  type α[V] = { type α[+A] = FingerTree[V, A] }
  
  def deepL[V, A](l: Digit[V, A], m: FingerTree[V, Node[V, A]], r: Digit[V, A])(implicit M: Measured[V, A]): FingerTree[V, A] = {
    type FV[+A] = FingerTree[V, A]
    type DV[+A] = Digit[V, A]
    l match {
      case D0() => {
        val vl: ViewL[FV, Node[V, A]] = m.viewL
        vl match {
          case EmptyL => ToReduceOps[DV, A](r).asTree
          case ConsL(lHead, lTail) => Deep(lHead.toDigit, lTail, r)
        }
      }
      case _ => Deep(l, m, r)
    }
  }
  
  def append3[V, A](l: FingerTree[V, A], m: List[A], r: FingerTree[V, A])(implicit M: Measured[V, A]): FingerTree[V, A] = {
    import Implicits._
    import Syntax._
    type DV[+A] = Digit[V, A]
    implicit val DConsable: Consable[List[A], FingerTree[V, A]] = Consable(ReduceList.reduceR(_ +: _))
    implicit val DSnocable: Snocable[FingerTree[V, A], List[A]] = Snocable(ReduceList.reduceL(_ :+ _))
    (l, m, r) match {
      case (Empty(),             mm, rr                 ) => mm ++: rr
      case (ll,                  mm, Empty()            ) => ll :++ mm
      case (Single(v, x),        mm, rr                 ) => x  +: mm ++: rr
      case (ll,                  mm, Single(v, x)       ) => ll :++ mm :+ x
      case (Deep(_, ll, lm, lr), mm, Deep(_, rl, rm, rr)) => Deep(ll, append3(lm, nodes(ToReduceOps[DV, A](lr).asList ::: mm ::: ToReduceOps[DV, A](rl).asList), rm), rr)
      case _                                              => !!!
    }
  }
  
  def nodes[V, A](as: List[A])(implicit M: Measured[V, A]): List[Node[V, A]] = as match {
    case a::b       ::Nil => N2(a, b   )::Nil
    case a::b::c    ::Nil => N3(a, b, c)::Nil
    case a::b::c::d ::Nil => N2(a, b   )::N2(c, d)::Nil
    case a::b::c    ::xs  => N3(a, b, c)::nodes(xs)
  }
}
