package recfun

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
    def pascal(c: Int, r: Int): Int = {
      if(c == 0) 1 else if(c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r -1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def check_balance(chars: List[Char], opened: Int): Boolean = {
        if(opened < 0) false
        else if(chars.isEmpty) if(opened == 0) true else false
        else if(chars.head == '(') check_balance(chars.tail, opened+1)
             else if (chars.head == ')') check_balance(chars.tail, opened-1)
             else check_balance(chars.tail, opened)
      }
      check_balance(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(coins.isEmpty) 0
      else if(money == 0) 0
      else if(money - coins.head < 0) countChange(money, coins.tail)
      else if(money - coins.head == 0) 1 + countChange(money, coins.tail)
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
