package fingertree

trait Tree[+A]
case object Z extends Tree[Nothing]
case class O[A](value: A) extends Tree[A] 
case class S[A](child: Tree[Node[A]]) extends Tree[A] 

trait Node[+A]
case class N2[A](a: A, b: A      ) extends Node[A]
case class N3[A](a: A, b: A, c: A) extends Node[A]

object Data {
  val empty: Tree[Int] = Z
  val zeroLevel: Tree[Int] = O(1)
  val oneLevel: Tree[Int] = S(O(N3(1, 2, 3)))
  val twoLevel: Tree[Int] = S(S(O(N3(N2(1, 2), N2(3, 4), N2(5, 6)))))
  val threeLevel: Tree[Char] = S(S(S(O(N2(
      N3(N2('t', 'h'), N2('i', 's'), N2('i', 's'))            : Node[Node[Char]],
      N3(N3('n', 'o', 't'), N2('a', 't'), N3('r', 'e', 'e'))  : Node[Node[Char]])))))
}
