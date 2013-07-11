package fingertree

import scalaz.Monoid

trait FingerTree[V, +A] {
  import Implicits._
  
  def +:[B >: A](x: B)(implicit M: Measured[V, B]): FingerTree[V, B] = {
    (this: FingerTree[V, B]) match {
      case Empty()                        => Single(M.measure(x), x)
      case Single(v, y)                   => Deep(D1(x)   , Empty()                                       , D1(v, y)  )
      case Deep(D4(v, a, b, c, d), m, r)  => Deep(D2(x, a), N3(b, c, d) +: (m: FingerTree[V, Node[V, B]]) , r         )
      case Deep(l             , m, r)     => Deep(x +: l  , m                                             , r         )
    }
  }

  def :+[B >: A](x: B)(implicit M: Measured[V, B]): FingerTree[V, B] = (this: FingerTree[V, B]) match {
    case Empty()                        => Single(M.measure(x), x)
    case Single(v, x)                   => Deep(D1(x) , Empty()                                       , D1(x)   )
    case Deep(l, m, D4(v, a, b, c, d))  => Deep(l     , (m: FingerTree[V, Node[V, B]]) :+ N3(a, b, c) , D2(d, x))
    case Deep(l, m, r             )     => Deep(l     , m                                             , r :+ x  )
  }
  
  def ++[W >: V, B >: A](that: FingerTree[V, B])(implicit M: Measured[V, B]): FingerTree[V, B] = FingerTree.append3[V, B](this, Nil, that)
  
  def viewL(implicit M: Measured[V, A]): ViewL[({type X[+A]=FingerTree[V, A]})#X, A] = this match {
    case Empty()        => EmptyL
    case Single(v, x)   => ConsL[({type X[+A]=FingerTree[V, A]})#X, A](x, Empty())
    case Deep(l, m, r)  => ConsL[({type X[+A]=FingerTree[V, A]})#X, A](l.headL, FingerTree.deepL(l.tailL, m, r))
  }
  
  def viewR(implicit M: Measured[V, A]): ViewR[({type X[+A]=FingerTree[V, A]})#X, A] = this match {
    case Empty()        => EmptyR
    case Single(v, x)   => ConsR[({type X[+A]=FingerTree[V, A]})#X, A](Empty(), x)
    case Deep(l, m, r)  => ConsR[({type X[+A]=FingerTree[V, A]})#X, A](FingerTree.deepL(l, m, r.tailR), r.headR)
  }
}

case class Empty[V]() extends FingerTree[V, Nothing]

case class Single[V, +A](v: V, a: A) extends FingerTree[V, A]

case class Deep[V, A](l: Digit[V, A], m: FingerTree[V, Node[V, A]], r: Digit[V, A]) extends FingerTree[V, A]

object FingerTree {
  import Implicits._
  import Syntax._

  def deepL[V, A](l: Digit[V, A], m: FingerTree[V, Node[V, A]], r: Digit[V, A])(implicit M: Measured[V, Node[V, A]]): FingerTree[V, A] = {
    l match {
      case D0() => {
        val vl: ViewL[({type X[A] = FingerTree[V, A]})#X, Node[V, A]] = m.viewL
        m.viewL match {
          case EmptyL => ???
//            implicit def XXX(implicit xxx: Reduce[({type X[+A]=Digit[V, A]})#X]): Reduce[({type X[+A]=Digit[V, A]})#X] = xxx
//            ToReduceOps[({type X[+A]=Digit[V, A]})#X, A](r)(XXX).toTree
          case consL => Deep(consL.head.toDigit, consL.tail, r)
        }
      }
      case _ => Deep(l, m, r)
    }
  }
  
  def append3[V, A](l: FingerTree[V, A], m: List[A], r: FingerTree[V, A])(implicit M: Measured[V, A]): FingerTree[V, A] = {
    import Implicits._
    import Syntax._
    implicit val DConsable: Consable[List[A], FingerTree[V, A]] = Consable(Function.uncurried(ReduceList.reduceR(Function.uncurried((a => b => a +: b ): A => (=> FingerTree[V, A]) => FingerTree[V, A]))))
    implicit val DSconable: Sconable[FingerTree[V, A], List[A]] = Sconable(Function.uncurried(ReduceList.reduceL(Function.uncurried((a => b => a :+ b ): FingerTree[V, A] => A => FingerTree[V, A]))))
//    implicit def XXX(implicit xxx: Reduce[({type X[+A]=Digit[V, A]})#X]): Reduce[({type X[+A]=Digit[V, A]})#X] = xxx
    (l, m, r) match {
      case (Empty(), mm, rr)                        => mm ++: rr
      case (ll, mm, Empty())                        => ll :++ mm
      case (Single(v, x), mm, rr)                   => x  +: mm ++: rr
      case (ll, mm, Single(v, x))                   => ll :++ mm :+ x
      case (Deep(ll, lm, lr), mm, Deep(rl, rm, rr)) => ???//Deep(ll, append3(lm, nodes(ToReduceOps(lr)(ReduceDigit[V]).asList ::: mm ::: ToReduceOps(rl)(XXX).asList), rm), rr)
      case _                                        => !!!
    }
  }
  
  def nodes[V, A](as: List[A])(implicit M: Measured[V, A]): List[Node[V, A]] = as match {
    case a::b       ::Nil => N2(a, b   )::Nil
    case a::b::c    ::Nil => N3(a, b, c)::Nil
    case a::b::c::d ::Nil => N2(a, b   )::N2(c, d)::Nil
    case a::b::c    ::xs  => N3(a, b, c)::nodes(xs)
  }
}
