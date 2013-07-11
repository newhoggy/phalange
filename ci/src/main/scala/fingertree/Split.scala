package fingertree

import scala.language.higherKinds

case class Split[S[+_], +A](l: S[A], a: A, r: S[A])
