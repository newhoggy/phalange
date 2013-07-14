package fingertree

import scalaz._, Scalaz._, Tags._

class Tagger[U] {
  def apply[T](t : T): T @@ U = t.asInstanceOf[T @@ U]
}
