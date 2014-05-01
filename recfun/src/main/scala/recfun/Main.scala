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
      } else if (charList.tail.isEmpty) {
        if (charList.head == ')') unmatched + 1
        else if (charList.head == '(') unmatched - 1
        else unmatched
      } else {
        val numUnmatched = unmatchedParens(charList.tail, unmatched)
        
        if (numUnmatched < 0 || charList.head == '(' && numUnmatched < 1) -1
        else if (charList.head == ')') numUnmatched + 1
        else if (charList.head == '(') numUnmatched - 1
        else numUnmatched
      }
    }

    if (unmatchedParens(chars, 0) == 0) true
    else false
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
