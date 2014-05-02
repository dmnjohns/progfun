package recfun

import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (r == 0 || c == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def unmatchedParens(charList: List[Char], unmatched: Int): Int = {
      if (charList.isEmpty) {
        unmatched
      } else {
        val numUnmatched = unmatchedParens(charList.tail, unmatched)
        
        if (numUnmatched < 0 || charList.head == '(' && numUnmatched < 1) -1
        else if (charList.head == ')') numUnmatched + 1
        else if (charList.head == '(') numUnmatched - 1
        else numUnmatched
      }
    }

    unmatchedParens(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    //todo: deal with duplicates in list
    if (money == 0) {
      1
    } else if (money < 0 || coins.isEmpty) {
      0
    } else {
      val subAmount = money - coins.head

      countChange(money, coins.tail) + countChange(subAmount, coins)
    }
  }
}
