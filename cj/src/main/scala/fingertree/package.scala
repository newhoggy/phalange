package object fingertree {
  def !!!(): Nothing = throw new UnsupportedOperationException

  type Tagged[U] = { type Tag = U }
  
  type @@[T, U] = T with Tagged[U]
  
  def tag[U] = new Tagger[U]
   
  // Manual specialization needed here ... specializing apply above doesn't help
  def tag[U](i : Int) : Int @@ U = i.asInstanceOf[Int @@ U]
  def tag[U](l : Long) : Long @@ U = l.asInstanceOf[Long @@ U]
  def tag[U](d : Double) : Double @@ U = d.asInstanceOf[Double @@ U]

  type Size = Int @@ SizeTag
  type Elem[A] = A @@ ElemTag
}
