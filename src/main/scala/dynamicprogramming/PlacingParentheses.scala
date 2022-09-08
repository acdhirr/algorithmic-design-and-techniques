package dynamicprogramming

import java.util.Scanner

object PlacingParentheses {

  def main(args: Array[String]): Unit = {

    val scan = new Scanner(System.in)
    val exp = scan.next()

    /*
    input: 5-8+7*4-8+9
    digits = (5, 8, 7, 4, 8, 9)
    operands = (-, +, *, -, +)
    */

    val z = exp.zipWithIndex
    val p = z.partition(_._2 % 2 == 0)
    val digits = p._1.map(_._1.asDigit).toArray
    val operands = p._2.map(_._1).toArray

    println(maximize (digits, operands))

  }

  def maximize(d: Array[Int], ops: Array[Char]): Long = {

    val n = d.length - 1 // 0..n-1  123456 0..5

    val mins = Array.fill(n + 1)(Array.fill(n + 1)(Long.MaxValue))
    val maxs = Array.fill(n + 1)(Array.fill(n + 1)(Long.MinValue))

    // fill diagonal of both grids with just the digits
    for (i <- 0 to n) {
      mins(i)(i) = d(i)
      maxs(i)(i) = d(i)
    }

    /*
      (i,j) = (0,1) (0,2) (0,3) (0,4) (0,5)
                    (1,2) (1,3) (1,4) (1,5)
                          (2,3) (2,4) (2,5)
                                (3,4) (3,5)
                                      (4,5)
      follow diagonals from upper left to lower right
    */
    for (s <- 1 to n; i <- 0 to n - s; j = i + s) {

      for (k <- i to j - 1) {

        val a = eval(maxs(i)(k), maxs(k + 1)(j), ops(k))
        val b = eval(maxs(i)(k), mins(k + 1)(j), ops(k))
        val c = eval(mins(i)(k), maxs(k + 1)(j), ops(k))
        val d = eval(mins(i)(k), mins(k + 1)(j), ops(k))

        mins(i)(j) = Set(mins(i)(j), a, b, c, d).min
        maxs(i)(j) = Set(maxs(i)(j), a, b, c, d).max
      }
    }

    // last column, first row contains answer
    maxs(0)(n)
  }

  // apply operand op to integers a and b
  def eval(a: Long, b: Long, op: Char): Long =

    op match {
      case '+' => a + b
      case '-' => a - b
      case '*' => a * b
      case _ => 0
    }

}
