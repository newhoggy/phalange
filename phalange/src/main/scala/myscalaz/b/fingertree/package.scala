package myscalaz.b

import scalaz.Foldable

package object fingertree {
//  implicit def fingerFoldable[V] = new Foldable[({type l[a]=Finger[V, a]})#l] with Foldable.FromFoldMap[({type l[a]=Finger[V, a]})#l] {
//    override def foldMap[A, M: Monoid](v: Finger[V, A])(f: A => M) = v.foldMap(f)
//  }

//  implicit def nodeFoldable[V] = new Foldable[({type l[a] = Node[a]})#l] {
//    def foldMap[A, M: Monoid](t: Node[V, A])(f: A => M): M = t foldMap f
//    def foldRight[A, B](v: Node[V, A], z: => B)(f: (A, => B) => B): B =
//       foldMap(v)((a: A) => (Endo.endo(f.curried(a)(_: B)))) apply z
//  }
}
