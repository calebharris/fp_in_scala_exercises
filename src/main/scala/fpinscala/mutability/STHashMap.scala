package fpinscala.mutability

import scala.collection.mutable

/**
  * @author caleb
  */
sealed abstract class STHashMap[S, K, V] {
  protected def value: mutable.HashMap[K, V]
}

object STHashMap {
  def apply[S, K, V](elems: (K, V)*): ST[S, STHashMap[S, K, V]] =
    ST(new STHashMap[S, K, V] {
      lazy val value = mutable.HashMap(elems: _*)
    })

  def apply[S, K, V](from: Map[K, V]): ST[S, STHashMap[S, K, V]] =
    ST(new STHashMap[S, K, V] {
      lazy val value = mutable.HashMap(from.toSeq: _*)
    })
}
