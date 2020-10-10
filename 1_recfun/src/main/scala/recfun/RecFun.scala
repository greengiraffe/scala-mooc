package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || r == c) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def inner (count: Int, chars: List[Char], last: Char): Boolean =
      if (chars.isEmpty) {
        if (last == '(') false
        else count == 0
      }
      else {
        if (chars.head == '(') inner(count + 1, chars.tail, '(')
        else if (chars.head == ')') inner(count - 1, chars.tail, ')')
        else inner(count, chars.tail, '?')
      }
    inner(0, chars, chars.head)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0)
      1
    else if(money > 0 && !coins.isEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
      0
  }
}
