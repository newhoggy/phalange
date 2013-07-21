package fingertree

import scala.language.higherKinds
import scala.language.implicitConversions

trait ToReduceOps {
  implicit def ToReduceOps[F[_], A](v: F[A])(implicit F0: Reduce[F]) = new ReduceOps[F, A] {
    def self = v
    implicit def F: Reduce[F] = F0
  }
}
