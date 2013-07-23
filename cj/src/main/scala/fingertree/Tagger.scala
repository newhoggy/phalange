package fingertree

import scala.language.higherKinds
import scalaz._, Scalaz._, Tags._

class Tagger[U] {
  def apply[T](t : T): T @@ U = t.asInstanceOf[T @@ U]
}

class TaggerF[U] {
  def apply[F[_], T](t : F[T]): F[T @@ U] = t.asInstanceOf[F[T @@ U]]
}
