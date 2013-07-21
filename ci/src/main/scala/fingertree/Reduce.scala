package fingertree

import scala.language.higherKinds

trait Reduce[F[_]]  {
  def reduceR[A, B](f: (A, B) => B)(fa: F[A], z: B): B
  def reduceL[A, B](f: (B, A) => B)(z: B, fa: F[A]): B

  def reduce2R[A, B, G[_]](f: (A, G[A]) => G[A])(implicit RG: Reduce[G]): (F[G[A]], G[A]) => G[A] = reduceR(RG.reduceR(f))
  def reduce2L[A, B, G[_]](f: (G[A], A) => G[A])(implicit RG: Reduce[G]): (G[A], F[G[A]]) => G[A] = reduceL(RG.reduceL(f))
}
