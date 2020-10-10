import week3._

val intList = new Cons(1, new Cons(2, new Cons(3, new Nil)))
intList.nth(1)
println(intList)

def nth[T](n: Int, xs: List[T]): T =  {
  if (xs.isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) xs.head
  else nth(n - 1, xs.tail)
}

println(nth(1, intList))
println(nth(-1, intList))