package fingertree

trait Node[V, +A] {
  def toDigit: Digit[V, A] = this match {
    case N2(v, a, b   ) => D2(v, a, b   )
    case N3(v, a, b, c) => D3(v, a, b, c)
  }
}

case class N2[V, +A](v: V, a: A, b: A       ) extends Node[V, A]
case class N3[V, +A](v: V, a: A, b: A, c: A ) extends Node[V, A]

object N2 {
  def apply[V, A](a: A, b: A)(implicit M: Measured[V, A]): N2[V, A] = N2(M.measure(a, b), a, b)
}

object N3 {
  def apply[V, A](a: A, b: A, c: A)(implicit M: Measured[V, A]): N3[V, A] = N3(M.measure(a, b, c), a, b, c)
}

object Node {
  type α[V] = { type α[+A] = Node[V, A] }
}
