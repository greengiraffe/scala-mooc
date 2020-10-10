def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}

sum(x => x*x*x, 1, 3)

def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)

// 1 * 4 * 9
product(x => x*x)(3,4)

def fact(n: Int) = product(x => x)(1,n)

fact(3)

def mapReduce(f: Int => Int, combine: (Int,Int) => Int, identity: Int)(a: Int, b: Int): Int =
  if (a > b) identity
  else
    combine(f(a), mapReduce(f, combine, identity)(a+1, b))

def newProduct(f: Int => Int) = mapReduce(f, (a, b) => a * b, 1)
def newSum(f: Int => Int) = mapReduce(f, (a, b) => a + b, 0)

mapReduce(x => x, (a,b) => a + b, identity = 0)(3,4)
mapReduce(x => x, (a,b) => a * b, identity = 1)(3,4)
