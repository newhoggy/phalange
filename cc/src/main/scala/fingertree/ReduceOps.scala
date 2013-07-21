package fingertree

import scala.language.higherKinds
import scalaz.syntax.Ops

trait ReduceOps[F[_], A] extends Ops[F[A]] {
  implicit def F: Reduce[F]
}
