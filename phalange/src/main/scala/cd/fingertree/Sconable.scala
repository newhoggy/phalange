package cd.fingertree

trait Sconable[S, A] { self =>
  def scon(sa: S, a: A): S
}
