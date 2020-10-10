package week3

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def nth(n: Int): T
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false

  def nth(n: Int): T = {
    def iter(i: Int, list: List[T]): T =
      if (i == n) list.head
      else iter(i + 1, list.tail)
    if (n < 0) throw new IndexOutOfBoundsException("n must be > 0 and was " + n)
    iter(0, this)
  }

  override def toString: String =
    head.toString() + "->" + tail.toString()
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
  def nth(n: Int) = throw new IndexOutOfBoundsException("Nil.nth")

  override def toString: String = "Nil"
}

object List {
  // List(1, 2) = List.apply(1, 2)
  def apply[T](x: T, y: T): List[T] = new Cons(x, new Cons(y, new Nil))

  def apply[T]() = new Nil

}
