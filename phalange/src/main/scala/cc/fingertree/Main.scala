package cc.fingertree

object Main {
  def main(args: Array[String]): Unit = {
    import Syntax._
    import Implicits._
    val x: FingerTree[Char] = Deep(D2('t', 'h'), Empty, D3('r', 'e', 'e'))
    val y: FingerTree[Char] = {
        Deep(
            D2('t', 'h'),
            Deep(
                D2(N2('i', 's'), N2('i', 's')),
                Empty,
                D2(N3('n', 'o', 't'), N2('a', 't'))),
            D3('r', 'e', 'e'))
    }
    println(y.toList)
    val z: FingerTree[Char] = 't' +: 'h' +: 'i' +: 's' +: 'i' +: 's' +: 'n' +: 'o' +: 't' +: 'a' +: 't' +: 'r' +: 'e' +: 'e' +: Empty
    println(y)
    println(z)
  }
}
