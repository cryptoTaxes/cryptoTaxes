package taxes.collection

import scala.collection.mutable.ListBuffer

case class Grouped[K, V](implicit ord: Ordering[K]) extends Iterable[(K, Seq[V])] {
  private var groups = collection.immutable.SortedMap[K, ListBuffer[V]]()

  def append(key: K, value: V): Unit = {
    val lb = groups.get(key) match {
      case None =>
        val lb = new ListBuffer[V]
        groups += (key -> lb)
        lb
      case Some(lb) =>
        lb
    }
    lb.append(value)
  }

  override def iterator: Iterator[(K, Seq[V])] =
    groups.iterator

  def apply(key: K): Seq[V] =
    groups(key)
}
