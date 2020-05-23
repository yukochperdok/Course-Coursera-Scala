package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 10, // was 40
    Key.exec.maxWarmupRuns -> 20, // was 80
    Key.exec.benchRuns -> 20, // was 120
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000 // was 100000000
    val chars = new Array[Char](length)
    val threshold = 100 // was 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def checkParentheses(chars: Array[Char], opened: Int): Boolean = {
      if (chars.isEmpty) opened == 0
      else if (opened < 0) false
      else if (chars.head == '(') checkParentheses(chars.tail, opened + 1)
      else if (chars.head == ')') checkParentheses(chars.tail, opened - 1)
      else checkParentheses(chars.tail, opened)
    }
    checkParentheses(chars,0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    @tailrec
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      if (idx == until) (arg1, arg2) /* (Opened, Closed) */
      else
        chars(idx) match {
          case '(' =>
            traverse(idx + 1, until, arg1 + 1, arg2)
          case ')' =>
            traverse(idx + 1, until, arg1, arg2 + 1)
          case _ => traverse(idx + 1, until, arg1, arg2)
        }
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val half = from + (until - from) / 2
        val ((openedL, closedL),(openedR, closedR)) = parallel(reduce(from, half), reduce(half, until))
        (openedL + openedR, closedL + closedR)
      }
    }

    val (opened, closed) = reduce(0, chars.length)
    opened == closed
  }

}
