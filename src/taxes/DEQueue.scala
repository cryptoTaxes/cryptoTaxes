package taxes

object DEQueue {
  def apply[T]() = new DEQueue[T]()

  def apply[T](x : T) = {
    val q = new DEQueue[T]()
    q.addLast(x)
    q
  }
}


class DEQueue[T] {
  private var xs = List[T]()

  def isEmpty : Boolean = xs.isEmpty

  def nonEmpty : Boolean = xs.nonEmpty

  def first : T = xs.head

  def last : T = xs.last

  def addFirst(x : T): Unit = {
    xs ::= x
  }

  def addLast(x : T): Unit = {
    xs = xs :+ x
  }

  def addLast(x : T, eq : (T,T) => Boolean, combine : (T,T) => T): Unit = {
    if(xs.nonEmpty && eq(last,x))
      xs = xs.init :+ combine(xs.last, x)
    else
      xs = xs :+ x
  }

  def removeFirst(): Unit = {
    xs = xs.tail
  }

  def removeLast(): Unit = {
    xs = xs.init
  }

  def iterator: Iterator[T] = xs.reverse.iterator

  def foreach(f: (T) ⇒ Unit): Unit = {
    xs.reverse.foreach(f)
  }

  override def toString =
    xs.reverse.mkString("DEQueue(", ", ", ")")
}
