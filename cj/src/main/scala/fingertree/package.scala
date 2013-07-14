import scalaz._, Tags._

package object fingertree {
  def !!!(): Nothing = throw new UnsupportedOperationException

  def tag[U] = new Tagger[U]
   
  // Manual specialization needed here ... specializing apply above doesn't help
  def tag[U](i : Int) : Int @@ U = i.asInstanceOf[Int @@ U]
  def tag[U](l : Long) : Long @@ U = l.asInstanceOf[Long @@ U]
  def tag[U](d : Double) : Double @@ U = d.asInstanceOf[Double @@ U]
}
