package cg.fingertree

trait Reduce[F[_]]  { self =>
  def reduceR[A, B](f: (A, => B) => B)(fa: F[A])( z: => B): B
  def reduceL[A, B](f: (B,    A) => B)(z:    B )(fa: F[A]): B
  
  def reduceRc[A, B]: ((A, => B) => B) => F[A] => (=> B) => B = reduceR[A, B](_)
  def reduceLc[A, B]: ((B,    A) => B) =>   B  =>  F[A]  => B = reduceL[A, B](_)

  def reduce2R[A, B](f: (A   , => F[A]) => F[A]): F[F[A]] => (=> F[A]) => F[A] = reduceRc((fa, fb) => reduceRc(f)(fa)(fb))
  def reduce2L[A, B](f: (F[A],      A ) => F[A]):   F[A]  =>   F[F[A]] => F[A] = reduceLc((fa, fb) => reduceLc(f)(fa)(fb))
}

object Moo {
  def f(a: Int)(b: Int)(c:Int): Int = a + b + c
  val x = f(1)(2)(3)
  val y = f(_)
  val z = y(2)(3)(4)
}