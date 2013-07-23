package fingertree

import scalaz.Monoid
import scalaz._, Scalaz._, Tags._

trait Implicits {
  import Syntax._
  
  implicit object ReduceSeq extends Reduce[Seq] {
    override def reduceR[A, B](f: (A, B) => B)(fa: Seq[A], z: B): B = fa.foldRight(z)(f)
    override def reduceL[A, B](f: (B, A) => B)(z: B, fa: Seq[A]): B = fa.foldLeft (z)(f)
  }
  
  implicit object ReduceList extends Reduce[List] {
    override def reduceR[A, B](f: (A, B) => B)(fa: List[A], z: B): B = fa.foldRight(z)(f)
    override def reduceL[A, B](f: (B, A) => B)(z: B, fa: List[A]): B = fa.foldLeft (z)(f)
  }
  
  implicit def ReduceFingerTree[V]: Reduce[FingerTree.α[V]#α] = new Reduce[FingerTree.α[V]#α] {
    import Syntax._
    override def reduceR[A, B](f: (A, B) => B)(fa: FingerTree[V, A], z: B): B = {
      implicit val DConsable = Consable(ReduceDigit[V].reduceR(f))
      implicit val FConsable = Consable(ReduceFingerTree[V].reduceR(ReduceNode[V].reduceR(f)))
      fa match {
        case Empty() => z
        case Single(v, a) => f(a, z)
        case Deep(_, l, m: FingerTree[V, Node[V, A]], r) => l +: m +: r +: z
      }
    }
    override def reduceL[A, B](f: (B, A) => B)(z: B, fa: FingerTree[V, A]): B =  {
      implicit val DSnocable = Snocable(ReduceDigit[V].reduceL(f))
      implicit val FSnocable = Snocable(ReduceFingerTree[V].reduceL(ReduceNode[V].reduceL(f)))
      fa match {
        case Empty() => z
        case Single(v, a) => f(z, a)
        case Deep(_, l, m: FingerTree[V, Node[V, A]], r) => z :+ l :+ m :+ r
      }
    }
  }

  implicit def ReduceDigit[V]: Reduce[Digit.α[V]#α] = new Reduce[Digit.α[V]#α] {
    override def reduceR[A, B](f: (A, B) => B)(fa: Digit[V, A], z: B): B = {
      implicit val BConsable = Consable(f)
      fa match {
        case D0(             ) =>                z
        case D1(v, a         ) =>           a +: z
        case D2(v, a, b      ) =>      a +: b +: z
        case D3(v, a, b, c   ) => a +: b +: c +: z
        case D4(v, a, b, c, d) => !!!
      }
    }
    
    override def reduceL[A, B](f: (B, A) => B)(z: B, fa: Digit[V, A]): B = {
      implicit val BSnocable = Snocable(f)
      fa match {
        case D0(             ) => z
        case D1(v, a         ) => z :+ a
        case D2(v, a, b      ) => z :+ a :+ b
        case D3(v, a, b, c   ) => z :+ a :+ b :+ c
        case D4(v, a, b, c, d) => !!!
      }
    }
  }

  implicit def ReduceNode[V]: Reduce[Node.α[V]#α] = new Reduce[Node.α[V]#α] {
    import Syntax._
    override def reduceR[A, B](f: (A, B) => B)(fa: Node[V, A], z: B): B = {
      implicit val BConsable = Consable(f)
      fa match {
        case N2(v, a, b      ) => a +: b +:      z
        case N3(v, a, b, c   ) => a +: b +: c +: z
      }
    }
    override def reduceL[A, B](f: (B, A) => B)(z: B, fa: Node[V, A]): B = {
      implicit val BSnocable = new Snocable[B, A] {
        override def snoc(sa: B, a: A): B = f(sa, a)
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
      case D0(             ) => M.monoid.zero
      case D1(v, _         ) => v
      case D2(v, _, _      ) => v
      case D3(v, _, _, _   ) => v
      case D4(v, _, _, _, _) => v
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
  
  implicit object MonoidSize extends Monoid[Int] {
    override def zero: Int = 0
    override def append(a: Int, b: => Int) = a + b
  }
  
  implicit def MeasuredElemSize[V] = new Measured[Int, V@@Elem] {
    override implicit def monoid: Monoid[Int] = MonoidSize

    override def measure(tree: V@@Elem): Int = 1
  }
}

object Implicits extends Implicits
