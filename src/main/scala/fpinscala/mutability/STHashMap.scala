package fpinscala.mutability

import scala.collection.mutable

/**
  * @author caleb
  */
sealed abstract class STHashMap[S, K, V] {
  protected def value: mutable.HashMap[K, V]

  def freeze: ST[S, Map[K, V]] = ST(value.toMap)

  def getOrElse(key: K, default: => V): ST[S, V] =
    ST(value.getOrElse(key, default))

  def update(key: K, elem: V): ST[S, Unit] = (s: S) => {
    value(key) = elem
    ((), s)
  }
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
