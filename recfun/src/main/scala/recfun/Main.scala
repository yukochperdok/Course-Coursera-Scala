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
    def pascal(c: Int, r: Int): Int =
      (c,r) match {
        case (_,0) => 1
        case (0,_) => 1
        case (col,row) if col==row => 1
        case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceStack(chars: List[Char], stack: List[Char]): Boolean = {
        chars match {
          case (Nil) => stack.isEmpty
          case '('::xs => balanceStack(xs, '('::stack)
          case ')'::xs => if (!stack.isEmpty) balanceStack(xs, stack.tail) else false
          case _ => balanceStack(chars.tail, stack)
        }
      }
      balanceStack(chars,List.empty)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 0
      else
        money match {
          case x if (x < 0) => 0
          case y if (y == 0) => 1
          case _ => countChange(money - coins.head, coins) + countChange(money, coins.tail)
        }
    }
  }
