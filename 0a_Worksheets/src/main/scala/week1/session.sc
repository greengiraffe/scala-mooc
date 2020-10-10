def abs(x: Double) = if (x < 0) -x else x

def sqrt(x: Double) = {
  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def isGoodEnough(guess: Double) =
    abs(guess * guess - x) / x < 0.001

  def improve(guess: Double) =
    (guess + x / guess) / 2

  sqrtIter(1.0)
}

sqrt(0.001)
sqrt(0.1e-20)
sqrt(1.0e20)
sqrt(1.0e60)

def factorial(x: Int): Int = {
  val n = x - 1
  def inner(n: Int) =
    if (n == 1) 1
    else n

  else x * inner(n)
}


factorial(3)