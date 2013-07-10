package fingertree

trait FingerTree[+V, +A] {
  import FingerTree.MN
  
  def +:[W >: V, B >: A](x: B)(implicit M: Measured[W, B]): FingerTree[W, B] = {
    (this: FingerTree[W, B]) match {
      case Empty                          => Single(M.measure(x), x)
      case Single(v, y)                   => Deep(D1(x)   , Empty                                         , D1(v, y)  )
      case Deep(D4(v, a, b, c, d), m, r)  => Deep(D2(x, a), N3(b, c, d) +: (m: FingerTree[W, Node[W, B]]) , r         )
      case Deep(l             , m, r)     => Deep(x +: l  , m                                             , r         )
    }
  }

  def :+[W >: V, B >: A](x: B)(implicit M: Measured[W, B]): FingerTree[W, B] = (this: FingerTree[W, B]) match {
    case Empty                          => Single(M.measure(x), x)
    case Single(v, x)                   => Deep(D1(x) , Empty                                         , D1(x)   )
    case Deep(l, m, D4(v, a, b, c, d))  => Deep(l     , (m: FingerTree[W, Node[W, B]]) :+ N3(a, b, c) , D2(d, x))
    case Deep(l, m, r             )     => Deep(l     , m                                             , r :+ x  )
  }
  
  def ++[W >: V, B >: A](that: FingerTree[W, B])(implicit M: Measured[W, B]): FingerTree[W, B] = FingerTree.append3[W, B](this, Nil, that)
  
  def viewL(implicit M: Measured[V, A]): ViewL[({type X[+A]=FingerTree[V, A]})#X, A] = this match {
    case Empty => EmptyL
    case Single(v, x) => ConsL[({type X[+A]=FingerTree[V, A]})#X, A](x, Empty)
    case Deep(l, m, r) => ConsL[({type X[+A]=FingerTree[V, A]})#X, A](l.headL, FingerTree.deepL(l.tailL, m, r))
  }
  
  def viewR(implicit M: Measured[V, A]): ViewR[({type X[+A]=FingerTree[V, A]})#X, A] = this match {
    case Empty => EmptyR
    case Single(v, x) => ConsR[({type X[+A]=FingerTree[V, A]})#X, A](Empty, x)
    case Deep(l, m, r) => ConsR[({type X[+A]=FingerTree[V, A]})#X, A](FingerTree.deepL(l, m, r.tailR), r.headR)
  }
}

case object Empty extends FingerTree[Nothing, Nothing]

case class Single[+V, +A](v: V, a: A) extends FingerTree[V, A]

case class Deep[+V, A](l: Digit[V, A], m: FingerTree[V, Node[V, A]], r: Digit[V, A]) extends FingerTree[V, A]

object FingerTree {
  import Implicits._
  import Syntax._

  def deepL[V, A](l: Digit[V, A], m: FingerTree[V, Node[V, A]], r: Digit[V, A])(implicit M: Measured[V, Node[V, A]]): FingerTree[V, A] = {
    l match {
      case D0 => m.viewL match {
        case EmptyL => r.toTree
        case ConsL(ma, mm) => Deep(ma.toDigit, mm, r) 
      }
      case _ => Deep(l, m, r)
    }
  }
  
  def append3[V, A](l: FingerTree[V, A], m: List[A], r: FingerTree[V, A])(implicit M: Measured[V, A]): FingerTree[V, A] = {
    import Implicits._
    import Syntax._
    implicit val DConsable: Consable[List[A], FingerTree[V, A]] = Consable(Function.uncurried(ReduceList.reduceR(Function.uncurried((a => b => a +: b ): A => (=> FingerTree[V, A]) => FingerTree[V, A]))))
    implicit val DSconable: Sconable[FingerTree[V, A], List[A]] = Sconable(Function.uncurried(ReduceList.reduceL(Function.uncurried((a => b => a :+ b ): FingerTree[V, A] => A => FingerTree[V, A]))))
    (l, m, r) match {
      case (Empty, mm, rr)                          => mm ++: rr
      case (ll, mm, Empty)                          => ll :++ mm
      case (Single(v, x), mm, rr)                      => x  +: mm ++: rr
      case (ll, mm, Single(v, x))                      => ll :++ mm :+ x
      case (Deep(ll, lm, lr), mm, Deep(rl, rm, rr)) => Deep(ll, append3(lm, nodes(lr.asList ::: mm ::: rl.asList), rm), rr)
      case _                                        => !!!
    }
  }
  
  def nodes[V, A](as: List[A])(implicit M: Measured[V, A]): List[Node[V, A]] = as match {
    case a::b       ::Nil => N2(a, b   )::Nil
    case a::b::c    ::Nil => N3(a, b, c)::Nil
    case a::b::c::d ::Nil => N2(a, b   )::N2(c, d)::Nil
    case a::b::c    ::xs  => N3(a, b, c)::nodes(xs)
  }

  implicit def MN[V, A](implicit M: Measured[V, A]): Measured[V, Node[V, A]] = new Measured[V, Node[V, A]] {
    override def measure(n: Node[V, A]): V = n match {
      case N2(v, _, _) => v
      case N3(v, _, _, _) => v
    }
  }
  
  implicit def MD[V, A](implicit M: Measured[V, A]): Measured[V, Digit[V, A]] = new Measured[V, Digit[V, A]] {
    override def measure(n: Digit[V, A]): V = n match {
      case D0                 => M.zero
      case D1(v, _)           => v
      case D2(v, _, _)        => v
      case D3(v, _, _, _)     => v
      case D4(v, _, _, _, _)  => v
    }
  }
}
