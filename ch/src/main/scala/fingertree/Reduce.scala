package fingertree

import scala.language.higherKinds

trait Reduce[F[_]]  {
  def reduceR[A, B](f: (A, B) => B)(fa: F[A],  z:   B ): B
  def reduceL[A, B](f: (B, A) => B)(z:    B , fa: F[A]): B

  def reduce2R[A, B](f: (A   , F[A]) => F[A]): (F[F[A]],   F[A] ) => F[A] = reduceR(reduceR(f))
  def reduce2L[A, B](f: (F[A],   A ) => F[A]): (  F[A] , F[F[A]]) => F[A] = reduceL(reduceL(f))
}
