package fingertree

object Implicits {
  implicit object ReduceList extends Reduce[List] {
    override def reduceR[A, B](f: (A, => B) => B)(fa: List[A])(z: => B): B = fa.foldRight(z)(f(_, _))
    override def reduceL[A, B](f: (B,    A) => B)(z:    B)(fa: List[A]): B = fa.foldLeft(z)(f)
  }
  
  implicit def ReduceFingerTree[V]: Reduce[({type X[A]=FingerTree[V, A]})#X] = new Reduce[({type X[A]=FingerTree[V, A]})#X] {
    import Syntax._
    final def mapDN[A, B, C](df: Digit[V, A] => B => C): Node[V, A] => B => C = df compose ((n: Node[V, A]) => n.toDigit)
    final def mapDN2[A, B, C](df: B => Digit[V, A] => C): B => Node[V, A] => C = { b: B => df(b) compose ((n: Node[V, A]) => n.toDigit) }
    override def reduceR[A, B](f: (A, => B) => B)(fa: FingerTree[V, A])(z: => B): B = {
      implicit val DConsable: Consable[Digit[V, A], B]                = Consable(Function.uncurried(ReduceDigit.reduceR(f)))
      implicit val FConsable: Consable[FingerTree[V, Node[V, A]], B]  = Consable(Function.uncurried(ReduceFingerTree[V].reduceR(Function.uncurried(mapDN(ReduceDigit.reduceR(f))))))
      fa match {
        case Empty() => z
        case Single(v, a) => f(a, z)
        case Deep(l, m: FingerTree[V, Node[V, A]], r) => l +: m +: r +: z
      }
    }
    override def reduceL[A, B](f: (B,    A) => B)(z:    B)(fa: FingerTree[V, A]): B =  {
      implicit val DSconable: Sconable[B, Digit[V, A]]                = Sconable(Function.uncurried(ReduceDigit.reduceL(f)))
      implicit val FSconable: Sconable[B, FingerTree[V, Node[V, A]]]  = Sconable(Function.uncurried(ReduceFingerTree.reduceL(Function.uncurried(mapDN2(ReduceDigit.reduceL(f))))))
      fa match {
        case Empty() => z
        case Single(v, a) => f(z, a)
        case Deep(l, m: FingerTree[V, Node[V, A]], r) => z :+ l :+ m :+ r
      }
    }
  }

  implicit def ReduceDigit[V]: Reduce[({type X[+A]=Digit[V, A]})#X] = new Reduce[({type X[A]=Digit[V, A]})#X] {
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

  implicit def ReduceNode[V]: Reduce[({type X[A]=Node[V, A]})#X] = new Reduce[({type X[A]=Node[V, A]})#X] {
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
}
