package fingertree

import scalaz.Monoid
import scalaz._, Scalaz._

object Main {
  def main(args: Array[String]): Unit = {
    import Syntax._
    import Implicits._

    val x: FingerTree[Int, Char] = Deep(D2('t', 'h'), Empty(), D3('r', 'e', 'e'))
    val y: FingerTree[Int, Char] = {
        Deep(
            D2('t', 'h'),
            Deep(
                D2(N2('i', 's'), N2('i', 's')),
                Empty(),
                D2(N3('n', 'o', 't'), N2('a', 't'))),
            D3('r', 'e', 'e'))
    }
    println(List(1, 2, 3, 4).asList)
    val z: FingerTree[Int, Char] = 't' +: 'h' +: 'i' +: 's' +: 'i' +: 's' +: 'n' +: 'o' +: 't' +: 'a' +: 't' +: 'r' +: 'e' +: 'e' +: Empty[Int]()
    println(y)
    println(z)
    println(('t'::'h'::'i'::'s'::'i'::'s'::'n'::'o'::'t'::'a'::'t'::'r'::'e'::'e'::Nil).toTree[Int])
    println(ToReduceOps[FingerTree.α[Int]#α, Char](y).asList)
    println(ToMeasuredOps(y).measure)
    println(y.split(_ >= 5)(0))
  }
}
