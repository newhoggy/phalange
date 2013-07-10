package object fingertree {
  def !!!(): Nothing = throw new UnsupportedOperationException
  
  type Node_V[A] = ({type X[V]=Node[V, A]})#X[A]
}
