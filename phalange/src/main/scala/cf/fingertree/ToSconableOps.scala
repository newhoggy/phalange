package cf.fingertree

trait ToSconableOps {
  implicit def ToSconableOps[S, A](v: S)(implicit F0: Sconable[S, A]) = new SconableOps[S, A] {
    def self = v
    implicit def F: Sconable[S, A] = F0
  }
}
