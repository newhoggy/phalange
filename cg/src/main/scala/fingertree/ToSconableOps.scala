package fingertree

import scala.language.implicitConversions

trait ToSnocableOps {
  implicit def ToSnocableOps[S, A](v: S)(implicit F0: Snocable[S, A]) = new SnocableOps[S, A] {
    def self = v
    implicit def F: Snocable[S, A] = F0
  }
}
