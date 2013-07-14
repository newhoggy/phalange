package fingertree

import scalaz.Monoid
import scalaz._, Scalaz._

trait Implicits extends FingerTreeImplicits {
}

object Implicits extends Implicits
