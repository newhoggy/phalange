package fingertree

import scala.language.implicitConversions

trait ToConsableOps {
  implicit def ToConsableOps[A, S](v: S)(implicit F0: Consable[A, S]) = new ConsableOps[A, S] {
    def self = v
    implicit def F: Consable[A, S] = F0
  }
}
