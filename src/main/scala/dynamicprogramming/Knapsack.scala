package dynamicprogramming

import java.util.Scanner
import scala.collection.mutable

object Knapsack {

  def main(args: Array[String]): Unit = {

    val scan = new Scanner(System.in)

    val w = scan.nextInt() // knapsack capacity (W)
    val n = scan.nextInt() // number of gold bars
    val weights = new Array[Int](n)
    for (i <- 0 until n) weights(i) = scan.nextInt()

    println( knapsack(weights, n, w) )
  }

  // Iterative algorithm, dynamically build a table,
  // then return value in lower right corner.
  def knapsack(weights: Array[Int], i:Int, w:Int): Int = {

    val table = Array.ofDim[Int](i+1,w+1)
    val ws = 0 +: weights // prefix a zero on the weights list

    for (row <- 0 to i)
      for (col <- 0 to w)
        table(row)(col) =
          if (row == 0) 0
          else if (ws(row) > col) table(row-1)(col)
          else Math.max(
            table(row-1)(col),
            table(row-1)(col-ws(row)) + ws(row)
          )

    table(i)(w) // lower right element contains answer
  }

  // recursive (but not tailrec), with memoization
  // Does not pass the test (time limit exceeded)
  def knapsackMemo(weights: Array[Int], i: Int, w: Int, memo: mutable.Map[(Int, Int), Int]): Int = {

    if (!memo.contains(i, w)) memo((i, w)) =
      if (i < 0) 0
      else if (weights(i) > w) knapsackMemo(weights, i - 1, w, memo)
      else Math.max(
        knapsackMemo(weights, i - 1, w, memo),
        knapsackMemo(weights, i - 1, w - weights(i), memo) + weights(i) // value = weight
      )

    memo(i, w)
  }

}
