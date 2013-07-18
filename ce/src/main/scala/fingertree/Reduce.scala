package fingertree

import scala.language.higherKinds

trait Reduce[F[_]]  {
  def reduceR[A, B](f: (A, => B) => B)(fa: F[A],  z: => B): B
  def reduceL[A, B](f: (B,    A) => B)( z:   B , fa: F[A]): B
  
  def reduceRc[A, B]: ((A, => B) => B) => (F[A], => B) => B = reduceR[A, B](_)
  def reduceLc[A, B]: ((B,    A) => B) => (  B , F[A]) => B = reduceL[A, B](_)

  def reduce2R[A, B](f: (A   , => F[A]) => F[A]): (F[F[A]], => F[A]) => F[A] = reduceRc(reduceRc(f))
  def reduce2L[A, B](f: (F[A],      A ) => F[A]): (  F[A] , F[F[A]]) => F[A] = reduceLc(reduceLc(f))
}
