package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
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
    def balanceLoop(chars: Array[Char], currentBalance: Int): Boolean = {
      (chars, currentBalance) match {
      case (Array(), _) => currentBalance == 0
      case (_, _) if currentBalance < 0 => false
      case (_, _) => chars.head match {
        case '(' => balanceLoop(chars.tail, currentBalance + 1)
        case ')' => balanceLoop(chars.tail, currentBalance - 1)
        case _ => balanceLoop(chars.tail, currentBalance)
      }
    }}
//    balanceLoop(chars, 0)

    var idx = 0
    var currentBalance = 0
    while (idx < chars.length) {
      if (currentBalance < 0) return false
      if (chars(idx) == '(') currentBalance += 1
      else if (chars(idx) == ')') currentBalance -= 1
      idx += 1
    }
    return currentBalance == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    def convertParentheses(char: Char): (Int, Int) = char match {
      case '(' => (1, 0)
      case ')' => (0, 1)
      case _ => (0, 0)
    }

    def combineParentheses(left: (Int, Int), right: (Int, Int)): (Int, Int) = (left._1, right._2) match {
      case (leftOpen, rightClose) if leftOpen >= rightClose => (leftOpen - rightClose + right._1, left._2)
      case (leftOpen, rightClose) => (right._1, rightClose - leftOpen + left._2)
    }
/*
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): Array[(Int, Int)] = {
      chars.slice(idx, until) map (convertParentheses(_))
    }
*/
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
        var i = idx
        var openBalance = 0
        var closeBalance = 0
        while (i < until) {
          if (chars(i) == '(') openBalance += 1
          else if (chars(i) == ')') {
            if (openBalance > 0) openBalance -= 1
            else closeBalance += 1
          }
          i += 1
        }
        return (openBalance, closeBalance)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if ((until - from) <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val (firstResult, secondResult) = parallel(reduce(from, mid), reduce(mid, until))
        combineParentheses(firstResult, secondResult)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
