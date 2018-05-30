package fpinscala

import scala.language.higherKinds

/**
  * @author caleb
  */
package object free {
  type ~>[F[_], G[_]] = Translate[F, G]
}
