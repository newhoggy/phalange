package fingertree

import scalaz.Monoid
import scalaz._, Scalaz._

trait Implicits extends FingerTreeImplicits {
  implicit def SizeMeasured[A] = new Measured[Size, A] {
    override def measure(a: A): Size = ???
    override def monoid: Monoid[Size] = ???
  }

  implicit object MeasuredSize extends Measured[Int, Char] {
    override implicit val monoid: Monoid[Int] = new Monoid[Int] {
      override def zero: Int = 0
      override def append(a: Int, b: => Int): Int = a + b
    }

    override def measure(a: Char): Int = 1
  }
}

object Implicits extends Implicits
