package fingertree

object Implicits {
  implicit object ReduceList extends Reduce[List] {
    override def reduceR[A, B](f: (A, => B) => B)(fa: List[A], z: => B): B = fa.foldRight(z)(f(_, _))
    override def reduceL[A, B](f: (B,    A) => B)(z:    B, fa: List[A]): B = fa.foldLeft(z)(f)
  }
  
  implicit object ReduceFingerTree extends Reduce[FingerTree] {
    import Syntax._
    final def mapDN[A, B, C](df: (Digit[A], B) => C): (Node[A], B) => C = { (n: Node[A], b: B) => df(n.toDigit, b) }
    final def mapDN2[A, B, C](df: (B, Digit[A]) => C): (B, Node[A]) => C = { (b: B, n: Node[A]) => df(b, n.toDigit) }
    override def reduceR[A, B](f: (A, => B) => B)(fa: FingerTree[A], z: => B): B = {
      implicit val DConsable = Consable(ReduceDigit.reduceR(f))
      implicit val FConsable = Consable(ReduceFingerTree.reduceR(mapDN(ReduceDigit.reduceR(f))))
      fa match {
        case Empty => z
        case Single(a) => f(a, z)
        case Deep(l, m, r) => l +: m +: r +: z
      }
    }
    override def reduceL[A, B](f: (B,    A) => B)(z:    B, fa: FingerTree[A]): B =  {
      implicit val DSconable = Sconable(ReduceDigit.reduceL(f))
      implicit val FSconable = Sconable(ReduceFingerTree.reduceL(mapDN2(ReduceDigit.reduceL(f))))
      fa match {
        case Empty => z
        case Single(a) => f(z, a)
        case Deep(l, m, r) => z :+ l :+ m :+ r
      }
    }
  }

  implicit object ReduceDigit extends Reduce[Digit] {
    override def reduceR[A, B](f: (A, => B) => B)(fa: Digit[A], z: => B): B = fa match {
      case D1(a         ) =>           f(a, z)
      case D2(a, b      ) =>      f(a, f(b, z))
      case D3(a, b, c   ) => f(a, f(b, f(c, z)))
      case D4(a, b, c, d) => !!!
    }
    override def reduceL[A, B](f: (B,    A) => B)(z:    B, fa: Digit[A]): B = fa match {
      case D1(a         ) =>     f(z, a)
      case D2(a, b      ) =>   f(f(z, a), b)
      case D3(a, b, c   ) => f(f(f(z, a), b), c)
      case D4(a, b, c, d) => !!!
    }
  }

  implicit object ReduceNode extends Reduce[Node] {
    import Syntax._
    override def reduceR[A, B](f: (A, => B) => B)(fa: Node[A], z: => B): B = {
      implicit val BConsable = Consable(f)
      fa match {
        case N2(a, b      ) => a +: b +:      z
        case N3(a, b, c   ) => a +: b +: c +: z
      }
    }
    override def reduceL[A, B](f: (B,    A) => B)(z:    B, fa: Node[A]): B = {
      implicit val BSconable = new Sconable[B, A] {
        override def scon(sa: B, a: A): B = f(sa, a)
      }
      fa match {
        case N2(a, b      ) => z :+ a :+ b
        case N3(a, b, c   ) => z :+ a :+ b :+ c
      }
    }
  }
}
