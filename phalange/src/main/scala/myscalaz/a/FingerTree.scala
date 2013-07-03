package myscalaz.a

import scalaz.Semigroup

sealed abstract class Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]): B

  /**
   * Append the given element to the right
   *
   * @throws if the finger is `Four`.
   */
  def +:(a: => A): Finger[V, A]

  /**
   * Prepends the given element to the left
   *
   * @throws if the finger is `Four`.
   */
  def :+(a: => A): Finger[V, A]

  /** Replaces the first element of this finger with `a` */
  def |-:(a: => A): Finger[V, A]

  /** Replaces the last element of this finger with `a` */
  def :-|(a: => A): Finger[V, A]

  def lhead: A

  def ltail: Finger[V, A]

  def rhead: A

  def rtail: Finger[V, A]

//  def toTree: FingerTree[V, A]

//  def map[B, V2](f: A => B)(implicit m: Reducer[B, V2]): Finger[V2, B]

  /** Apply the given side effect to each element. */
  def foreach(f: A => Unit): Unit

  /** An iterator that visits each element. */
  def iterator: Iterator[A]

  /** An iterator that visits each element in reverse order. */
  def reverseIterator: Iterator[A]

  def measure: V

//  def toList: List[A] = map(x => x)(Reducer.ListReducer[A]).measure

//  private[scalaz] def split1(pred: V => Boolean, accV: V): (Option[Finger[V, A]], A, Option[Finger[V, A]])
}
