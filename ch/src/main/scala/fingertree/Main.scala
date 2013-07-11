package fingertree

import scalaz.Monoid

object Main {
  def main(args: Array[String]): Unit = {
    import Syntax._
    import Implicits._
    val x: FingerTree[Unit, Char] = Deep(D2(Unit, 't', 'h'), Empty(), D3(Unit, 'r', 'e', 'e'))
    val y: FingerTree[Unit, Char] = {
        Deep(
            D2(Unit, 't', 'h'),
            Deep(
                D2(Unit, N2(Unit, 'i', 's'), N2(Unit, 'i', 's')),
                Empty(),
                D2(Unit, N3(Unit, 'n', 'o', 't'), N2(Unit, 'a', 't'))),
            D3(Unit, 'r', 'e', 'e'))
    }
    println(List(1, 2, 3, 4).asList)
    implicit object M extends Measured[Unit, Char] {
      override implicit val monoid: Monoid[Unit] = new Monoid[Unit] {
        override def zero: Unit = Unit
        override def append(f1: Unit, f2: => Unit): Unit = Unit
      }

      override def measure(a: Char): Unit = Unit
    }
    val z: FingerTree[Unit, Char] = 't' +: 'h' +: 'i' +: 's' +: 'i' +: 's' +: 'n' +: 'o' +: 't' +: 'a' +: 't' +: 'r' +: 'e' +: 'e' +: Empty()
    println(y)
    println(z)
    println(('t'::'h'::'i'::'s'::'i'::'s'::'n'::'o'::'t'::'a'::'t'::'r'::'e'::'e'::Nil).toTree)
    println(ToReduceOps[({type X[+A]=FingerTree[Unit, A]})#X, Char](y).asList)
  }
}
