package taxes

trait Container[T] {
  protected var xs = List[T]()

  def isEmpty : Boolean =
    xs.isEmpty

  def nonEmpty : Boolean =
    xs.nonEmpty

  def first : T

  def removeFirst()

  def addLast(x : T)

  def addLast(x : T, eq : (T,T) => Boolean, combine : (T,T) => T)

  def iterator: Iterator[T] =
    xs.iterator

  def foreach(f: (T) ⇒ Unit): Unit = {
    xs.foreach(f)
  }
}


class Queue[T]() extends Container[T] {
  def first : T =
    xs.head

  def removeFirst(): Unit =
    xs = xs.tail

  def addLast(x : T): Unit =
    xs = xs :+ x

  def addLast(x : T, eq : (T,T) => Boolean, combine : (T,T) => T): Unit =
    if(xs.nonEmpty && eq(xs.last,x))
      xs = xs.init :+ combine(xs.last, x)
    else
      xs = xs :+ x

  override def toString =
    xs.mkString("Queue(", ", ", ")")
}


class Stack[T]() extends Container[T] {
  def first : T =
    xs.head

  def removeFirst(): Unit =
    xs = xs.tail

  def addLast(x : T): Unit =
    xs = x :: xs

  def addLast(x : T, eq : (T,T) => Boolean, combine : (T,T) => T): Unit =
    if(xs.nonEmpty && eq(xs.head,x))
      xs = combine(xs.head, x) :: xs.tail
    else
      xs = x :: xs

  override def toString =
    xs.mkString("Stack(", ", ", ")")
}
