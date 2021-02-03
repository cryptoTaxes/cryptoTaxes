package taxes.collection

class DoubleEndedContainer[T] extends Iterable[T] {
  private case class Node[E](var x: E, var next: Node[E])

  private var firstNode: Node[T] = null
  private var lastNode: Node[T] = null
  private var sz = 0

  override def isEmpty: Boolean =
    sz == 0

  override def nonEmpty: Boolean =
    sz > 0

  override def size: Int =
    sz

  def first: T =
    if(isEmpty)
      throw new NoSuchElementException("first on empty DoubleEndedContainer")
    else
      firstNode.x

  override def last: T =
    if(isEmpty)
      throw new NoSuchElementException("last on empty DoubleEndedContainer")
    else
      lastNode.x

  def prepend(x: T): Unit = {
    firstNode = Node(x, firstNode)
    if(sz==0)
      lastNode = firstNode
    sz += 1
  }

  def append(x: T): Unit = {
    val node = Node(x, null)
    if(isEmpty)
      firstNode = node
    else
      lastNode.next = node
    lastNode = node
    sz += 1
  }

  def removeFirst(): Unit = {
    if(isEmpty)
      throw new NoSuchElementException("removeFirst on empty DoubleEndedContainer")
    else
      firstNode = firstNode.next
    sz -= 1
  }

  def combineFirst(x: T, eq: (T,T) => Boolean, combine: (T,T) => T): Unit = {
    if(nonEmpty && eq(firstNode.x, x)) {
      firstNode.x = combine(firstNode.x, x)
    } else
      prepend(x)
  }

  def combineLast(x: T, eq: (T,T) => Boolean, combine: (T,T) => T): Unit = {
    if(nonEmpty && eq(lastNode.x, x))
      lastNode.x = combine(lastNode.x, x)
    else
      append(x)
  }

  protected class DoubleEndedIterator extends Iterator[T] {
    private var node = firstNode

    override def hasNext: Boolean =
      node != null

    override def next(): T = {
      if(!hasNext)
        throw new NoSuchElementException("next on empty DoubleEndedIterator")
      else {
        val x = node.x
        node = node.next
        return x
      }
    }
  }

  def iterator: Iterator[T] =
    new DoubleEndedIterator
}


trait Container[T] extends Iterable[T] {
  protected val doubleEndedContainer = new DoubleEndedContainer[T]()

  override def isEmpty: Boolean =
    doubleEndedContainer.isEmpty

  override def nonEmpty: Boolean =
    doubleEndedContainer.nonEmpty

  def first: T =
    doubleEndedContainer.first

  def removeFirst(): Unit =
    doubleEndedContainer.removeFirst()

  def insert(x: T)

  def insert(x: T, eq: (T,T) => Boolean, combine: (T,T) => T)

  def iterator: Iterator[T] =
    doubleEndedContainer.iterator
}


class Queue[T]() extends Container[T] {
  def insert(x: T): Unit =
    doubleEndedContainer.append(x)

  def insert(x: T, eq: (T,T) => Boolean, combine: (T,T) => T): Unit =
    doubleEndedContainer.combineLast(x, eq, combine)

  override def toString =
    doubleEndedContainer.mkString("Queue(", ", ", ")")
}


class Stack[T]() extends Container[T] {
  def insert(x: T): Unit =
    doubleEndedContainer.prepend(x)

  def insert(x: T, eq: (T,T) => Boolean, combine: (T,T) => T): Unit =
    doubleEndedContainer.combineFirst(x, eq, combine)

  override def toString =
    doubleEndedContainer.mkString("Stack(", ", ", ")")
}
