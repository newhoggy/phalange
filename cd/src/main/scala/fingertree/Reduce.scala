package fingertree

import scala.language.higherKinds

trait Reduce[F[_]]  { self =>
  def reduceR[A, B](f: (A, => B) => B)(fa: F[A])( z: => B): B
  def reduceL[A, B](f: (B,    A) => B)( z:    B)(fa: F[A]): B
}
