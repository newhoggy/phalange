package ca.fingertree

object Main {
  def main(args: Array[String]): Unit = {
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
  }
}
