object worksheet {
  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor = new Succ(this)
    def +(that: Nat): Nat
    def -(that: Nat): Nat
  }

  object Zero extends Nat {
    def isZero = true
    def predecessor = throw new Error("0.predecessor")
    def +(that: Nat) = that
    def -(that: Nat) =
      if (that.isZero) this
      else throw new Error("negative number")
  }

  class Succ(n: Nat) extends Nat {
    def isZero = false
    def predecessor = n
    def +(that: Nat) = new Succ(n + that)
    def -(that: Nat) =
      if (that.isZero) this
      else n - that.predecessor
  }

  val zero = Zero
  val one = new Succ(zero)
  val two = new Succ(one)

  assert(!one.isZero, "1 > 0")
  assert((one - one).isZero, "1 - 1 = 0")
  assert((two - one - one).isZero, "2 - 1 - 1 = 0")
  assert(!(two - one).isZero, "2 - 1 != 0")
  assert((one + one - two).isZero, "1 + 1 - 2 = 0")
  assert((one + new Succ(two) - two - one - one).isZero, "1 + 3 - 2 - 1 - 1= 0")

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }

  val list = List(4,7,1,3)
  val sorted = isort(list)
  println(sorted)

}
