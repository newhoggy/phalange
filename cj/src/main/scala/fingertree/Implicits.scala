package fingertree

import scalaz.Monoid
import scalaz._, Scalaz._, Tags._

trait Implicits {
  implicit object ReduceList extends Reduce[List] {
    override def reduceR[A, B](f: (A, => B) => B)(fa: List[A])(z: => B): B = fa.foldRight(z)(f(_, _))
    override def reduceL[A, B](f: (B,    A) => B)(z:    B)(fa: List[A]): B = fa.foldLeft(z)(f)
  }
  
  implicit def ReduceFingerTree[V]: Reduce[FingerTree.α[V]#α] = new Reduce[FingerTree.α[V]#α] {
    import Syntax._
    final def mapDN[A, B, C](df: Digit[V, A] => B => C): Node[V, A] => B => C = df compose ((n: Node[V, A]) => n.toDigit)
    final def mapDN2[A, B, C](df: B => Digit[V, A] => C): B => Node[V, A] => C = { b: B => df(b) compose ((n: Node[V, A]) => n.toDigit) }
    override def reduceR[A, B](f: (A, => B) => B)(fa: FingerTree[V, A])(z: => B): B = {
      implicit val DConsable: Consable[Digit[V, A], B]                = Consable(Function.uncurried(ReduceDigit.reduceR(f)))
      implicit val FConsable: Consable[FingerTree[V, Node[V, A]], B]  = Consable(Function.uncurried(ReduceFingerTree[V].reduceR(Function.uncurried(mapDN(ReduceDigit.reduceR(f))))))
      fa match {
        case Empty() => z
        case Single(v, a) => f(a, z)
        case Deep(_, l, m: FingerTree[V, Node[V, A]], r) => l +: m +: r +: z
      }
    }
    override def reduceL[A, B](f: (B,    A) => B)(z:    B)(fa: FingerTree[V, A]): B =  {
      implicit val DSconable: Sconable[B, Digit[V, A]]                = Sconable(Function.uncurried(ReduceDigit.reduceL(f)))
      implicit val FSconable: Sconable[B, FingerTree[V, Node[V, A]]]  = Sconable(Function.uncurried(ReduceFingerTree.reduceL(Function.uncurried(mapDN2(ReduceDigit.reduceL(f))))))
      fa match {
        case Empty() => z
        case Single(v, a) => f(z, a)
        case Deep(_, l, m: FingerTree[V, Node[V, A]], r) => z :+ l :+ m :+ r
      }
    }
  }

  implicit def ReduceDigit[V]: Reduce[Digit.α[V]#α] = new Reduce[Digit.α[V]#α] {
    override def reduceR[A, B](f: (A, => B) => B)(fa: Digit[V, A])(z: => B): B = fa match {
      case D1(v, a         ) =>           f(a, z)
      case D2(v, a, b      ) =>      f(a, f(b, z))
      case D3(v, a, b, c   ) => f(a, f(b, f(c, z)))
      case D4(v, a, b, c, d) => !!!
    }
    override def reduceL[A, B](f: (B,    A) => B)(z:    B)(fa: Digit[V, A]): B = fa match {
      case D1(v, a         ) =>     f(z, a)
      case D2(v, a, b      ) =>   f(f(z, a), b)
      case D3(v, a, b, c   ) => f(f(f(z, a), b), c)
      case D4(v, a, b, c, d) => !!!
    }
  }

  implicit def ReduceNode[V]: Reduce[Node.α[V]#α] = new Reduce[Node.α[V]#α] {
    import Syntax._
    override def reduceR[A, B](f: (A, => B) => B)(fa: Node[V, A])(z: => B): B = {
      implicit val BConsable = Consable(f)
      fa match {
        case N2(v, a, b      ) => a +: b +:      z
        case N3(v, a, b, c   ) => a +: b +: c +: z
      }
    }
    override def reduceL[A, B](f: (B,    A) => B)(z:    B)(fa: Node[V, A]): B = {
      implicit val BSconable = new Sconable[B, A] {
        override def scon(sa: B, a: A): B = f(sa, a)
      }
      fa match {
        case N2(v, a, b      ) => z :+ a :+ b
        case N3(v, a, b, c   ) => z :+ a :+ b :+ c
      }
    }
  }

  implicit def MeasuredNode[V, A](implicit M: Measured[V, A]): Measured[V, Node[V, A]] = new Measured[V, Node[V, A]] {
    override implicit def monoid: Monoid[V] = M.monoid

    override def measure(n: Node[V, A]): V = n match {
      case N2(v, _, _) => v
      case N3(v, _, _, _) => v
    }
  }
  
  implicit def MeasuredDigit[V, A](implicit M: Measured[V, A]): Measured[V, Digit[V, A]] = new Measured[V, Digit[V, A]] {
    override implicit def monoid: Monoid[V] = M.monoid

    override def measure(n: Digit[V, A]): V = n match {
      case D0()               => M.monoid.zero
      case D1(v, _)           => v
      case D2(v, _, _)        => v
      case D3(v, _, _, _)     => v
      case D4(v, _, _, _, _)  => v
    }
  }
  
  implicit def MeasuredFingerTree[V, A](implicit MD: Measured[V, A]): Measured[V, FingerTree[V, A]] = new Measured[V, FingerTree[V, A]] {
    import Syntax._

    override implicit def monoid: Monoid[V] = MD.monoid

    override def measure(tree: FingerTree[V, A]): V = tree match {
      case Empty() => monoid.zero
      case Single(v, _) => v
      case Deep(v, _, _, _) => v
    }
  }
  
  implicit object MonoidSize extends Monoid[Int@@Size] {
    override def zero: Int@@Size = Tag[Int, Size](0)
    override def append(a: Int@@Size, b: => Int@@Size) = Tag[Int, Size](a + b)
  }
  
  implicit object MeasuredElemSize extends Measured[Int@@Size, Nothing@@Elem] {
    override implicit def monoid: Monoid[Int@@Size] = MonoidSize

    override def measure(tree: Nothing@@Elem): Int@@Size = tag[Size](1)
  }
}

object Implicits extends Implicits
