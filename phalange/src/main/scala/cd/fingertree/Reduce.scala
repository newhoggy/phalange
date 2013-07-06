package cd.fingertree

trait Reduce[F[_]]  { self =>
  def reduceR[A, B](f: (A, => B) => B)(z: => B)(fa: F[A]): B
  def reduceL[A, B](f: (B,    A) => B)(z:    B)(fa: F[A]): B
}
