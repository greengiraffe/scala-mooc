object week3 {

  abstract class IntSet {
    def incl(x: Int): IntSet

    def contains(x: Int): Boolean

    def union(other: IntSet): IntSet
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

    def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this

    /**
     * this terminates because every call to union is done with a smaller
     * set until some union falls back to the union implementation of Empty
     * which does terminate (it simply returns the other set)
     *
     * (left union right) is a set without the parent elem
     * ((left union right) union other) also lacks the parent elem
     * that's why the parent elem is included again at the end
     */
    def union(other: IntSet) =
      ((left union right) union other) incl elem

    override def toString: String = "{" + left + elem + right + "}"
  }

  class Empty extends IntSet {
    def contains(x: Int): Boolean = false

    def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

    def union(other: IntSet) = other

    override def toString: String = "."
  }


  val s1 = new NonEmpty(5, new Empty, new Empty)
  val s2 = s1 incl 2 incl 6 incl 7
  val s3 = s1 incl 2 incl 1 incl 99
  val s4 = s2 union s3
  println(s2)
  println(s3)
  println(s4)

}