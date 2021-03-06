package fingertree

import Syntax._

object Implicits {
  implicit object ReduceList extends Reduce[List] {
    override def reduceR[A, B](f: (A, B) => B)(fa: List[A], z: B): B = fa.foldRight(z)(f)
    override def reduceL[A, B](f: (B, A) => B)(z: B, fa: List[A]): B = fa.foldLeft (z)(f)
  }
  
  implicit object ReduceFingerTree extends Reduce[FingerTree] {
    override def reduceR[A, B](f: (A, B) => B)(fa: FingerTree[A], z: B): B = {
      implicit val DConsable = Consable(ReduceDigit.reduceR(f))
      implicit val FConsable = Consable(ReduceFingerTree.reduceR(ReduceNode.reduceR(f)))
      fa match {
        case Empty => z
        case Single(a) => f(a, z)
        case Deep(l, m, r) => l +: m +: r +: z
      }
    }
    override def reduceL[A, B](f: (B, A) => B)(z: B, fa: FingerTree[A]): B = {
      implicit val DSnocable = Snocable(ReduceDigit.reduceL(f))
      implicit val FSnocable = Snocable(ReduceFingerTree.reduceL(ReduceNode.reduceL(f)))
      fa match {
        case Empty => z
        case Single(a) => f(z, a)
        case Deep(l, m, r) => z :+ l :+ m :+ r
      }
    }
  }

  implicit object ReduceDigit extends Reduce[Digit] {
    override def reduceR[A, B](f: (A, B) => B)(fa: Digit[A], z: B): B = {
      implicit val BConsable = Consable(f)
      fa match {
        case D1(a         ) =>                a +: z
        case D2(a, b      ) =>           a +: b +: z
        case D3(a, b, c   ) =>      a +: b +: c +: z
        case D4(a, b, c, d) => a +: b +: c +: z
      }
    }
    
    override def reduceL[A, B](f: (B, A) => B)(z: B, fa: Digit[A]): B = {
      implicit val BSnocable = Snocable(f)
      fa match {
        case D1(a         ) => z :+ a
        case D2(a, b      ) => z :+ a :+ b
        case D3(a, b, c   ) => z :+ a :+ b :+ c
        case D4(a, b, c, d) => z :+ a :+ b :+ c :+ d
      }
    }
  }

  implicit object ReduceNode extends Reduce[Node] {
    import Syntax._
    override def reduceR[A, B](f: (A, B) => B)(fa: Node[A], z: B): B = {
      implicit val BConsable = Consable(f)
      fa match {
        case N2(a, b   ) => a +: b +:      z
        case N3(a, b, c) => a +: b +: c +: z
      }
    }
    override def reduceL[A, B](f: (B, A) => B)(z: B, fa: Node[A]): B = {
      implicit val BSnocable = Snocable(f)
      fa match {
        case N2(a, b   ) => z :+ a :+ b
        case N3(a, b, c) => z :+ a :+ b :+ c
      }
    }
  }
}
