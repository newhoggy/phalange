package fingertree

import scala.language.higherKinds
import scalaz.Scalaz, Scalaz._
import scalaz.Monoid

trait FingerTree[V, +A] {
  type FV[+A] = FingerTree[V, A]
  type DV[+A] = Digit[V, A]
  type NV[+A] = Node[V, A]
  
  import Syntax._
  import Implicits._
  
  def +:[B >: A](x: B)(implicit M: Measured[V, B]): FingerTree[V, B] = {
    (this: FingerTree[V, B]) match {
      case Empty()                          => Single(M.measure(x), x)
      case Single(v, y)                     => Deep(D1(x)   , Empty()                                       , D1(v, y)  )
      case Deep(_, D4(_, a, b, c, d), m, r) => Deep(D2(x, a), N3(b, c, d) +: (m: FingerTree[V, Node[V, B]]) , r         )
      case Deep(_, l             , m, r)    => Deep(x +: l  , m                                             , r         )
    }
  }

  def :+[B >: A](x: B)(implicit M: Measured[V, B]): FingerTree[V, B] = (this: FingerTree[V, B]) match {
    case Empty()                          => Single(M.measure(x), x)
    case Single(_, x)                     => Deep(D1(x) , Empty()                                       , D1(x)   )
    case Deep(_, l, m, D4(v, a, b, c, d)) => Deep(l     , (m: FingerTree[V, Node[V, B]]) :+ N3(a, b, c) , D2(d, x))
    case Deep(_, l, m, r             )    => Deep(l     , m                                             , r :+ x  )
  }
  
  def ++[W >: V, B >: A](that: FingerTree[V, B])(implicit M: Measured[V, B]): FingerTree[V, B] = FingerTree.append3[V, B](this, Nil, that)
  
  def viewL(implicit M: Measured[V, A]): ViewL[FV, A] = this match {
    case Empty()          => EmptyL
    case Single(v, x)     => ConsL[FV, A](x, Empty())
    case Deep(_, l, m, r) => ConsL[FV, A](l.headL, FingerTree.deepL(l.tailL, m, r))
  }
  
  def viewR(implicit M: Measured[V, A]): ViewR[FV, A] = this match {
    case Empty()          => EmptyR
    case Single(v, x)     => ConsR[FV, A](Empty(), x)
    case Deep(_, l, m, r) => ConsR[FV, A](FingerTree.deepL(l, m, r.tailR), r.headR)
  }

  def splitTree(p: V => Boolean)(i: V)(implicit M: Measured[V, A]): Split[FV, A] = {
    implicit def MonoidV = M.monoid
    this match {
      case Empty()                                    => !!!
      case Single(_, a)                               => Split[FV, A](Empty(), a, Empty())
      case Deep(_, l, m, r) => {
        lazy val vl = i  |+| ToMeasuredOps(l).measure
        lazy val vm = vl |+| ToMeasuredOps(r).measure
        Unit match {
          case _ if p(vl) => (l.split(p)(i): Split[DV, A]) match {
            case Split(sl, sx, sr) => Split[FV, A](sl.toTree, sx, FingerTree.deepL(sr, m, r))
          }
          case _ if p(vm) => (m.splitTree(p)(vl): Split[FV, Node[V, A]]) match {
            case Split(ml, mm, mr) => {
              val vml = ToMeasuredOps(ml).measure
              (mm.toDigit.split(p)(vl |+| vml): Split[DV, A]) match {
                case Split(mml, mmx, mmr) => Split[FV, A](FingerTree.deepR(l, ml, mml), mmx, FingerTree.deepL(mmr, mr, r))
              }
            }
          }
          case _ => (r.split(p)(i): Split[DV, A]) match {
            case Split(rl, rm, rr) => Split[FV, A](FingerTree.deepR(l, m, rl), rm, rr.toTree)
          }
        }
      }
    }
  }

  def split(p: V => Boolean)(implicit M: Measured[V, A]): (FingerTree[V, A], FingerTree[V, A]) = this match {
    case Empty() => (Empty(), Empty())
    case _ if p(ToMeasuredOps(this).measure) => splitTree(p)(M.monoid.zero) match {
      case Split(l, m, r) => (l, m +: r)
    }
    case _ => (this, Empty())
  }

  def takeUntil(p: V => Boolean)(implicit M: Measured[V, A]): FingerTree[V, A] = split(p)._1

  def dropUntil(p: V => Boolean)(implicit M: Measured[V, A]): FingerTree[V, A] = split(p)._2
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

  def deepL[V, A](l: Digit[V, A], m: FingerTree[V, Node[V, A]], r: Digit[V, A])(implicit M: Measured[V, A]): FingerTree[V, A] = l match {
    case D0() => m.viewL match {
      case EmptyL => ToReduceOps[Digit.α[V]#α, A](r).toTree
      case consL => Deep(consL.lHead.toDigit, consL.lTail, r)
    }
    case _ => Deep(l, m, r)
  }
  
  def deepR[V, A](l: Digit[V, A], m: FingerTree[V, Node[V, A]], r: Digit[V, A])(implicit M: Measured[V, A]): FingerTree[V, A] = r match {
    case D0() => m.viewR match {
      case EmptyR => ToReduceOps[Digit.α[V]#α, A](l).toTree
      case consR => Deep(consR.rHead.toDigit, consR.rTail, l)
    }
    case _ => Deep(l, m, r)
  }
  
  def append3[V, A](l: FingerTree[V, A], m: List[A], r: FingerTree[V, A])(implicit M: Measured[V, A]): FingerTree[V, A] = {
    import Implicits._
    import Syntax._
    type DX[+A] = Digit[V, A]
    implicit val DConsable: Consable[List[A], FingerTree[V, A]] = Consable(Function.uncurried(ReduceList.reduceR(Function.uncurried((a => b => a +: b ): A => (=> FingerTree[V, A]) => FingerTree[V, A]))))
    implicit val DSconable: Sconable[FingerTree[V, A], List[A]] = Sconable(Function.uncurried(ReduceList.reduceL(Function.uncurried((a => b => a :+ b ): FingerTree[V, A] => A => FingerTree[V, A]))))
    (l, m, r) match {
      case (Empty(), mm, rr)                              => mm ++: rr
      case (ll, mm, Empty())                              => ll :++ mm
      case (Single(v, x), mm, rr)                         => x  +: mm ++: rr
      case (ll, mm, Single(v, x))                         => ll :++ mm :+ x
      case (Deep(_, ll, lm, lr), mm, Deep(_, rl, rm, rr)) => Deep(ll, append3(lm, nodes(ToReduceOps[DX, A](lr).asList ::: mm ::: ToReduceOps[DX, A](rl).asList), rm), rr)
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
