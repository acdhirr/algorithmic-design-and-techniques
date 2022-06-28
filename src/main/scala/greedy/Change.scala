package greedy

import java.util.Scanner
import scala.collection.mutable.Stack

object Change {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val m = s.nextInt()
    val denominations = Stack(10, 5, 1) // types of coins (values in cents)
    println(change(m, denominations, 0))
  }

  def change(m: Int, denominations: Stack[Int], iteration: Int): Int = {

    if (m == 0) iteration // ready, return number of coins
    else {
      // if remaining amount is smaller than current denomination, pop
      // that denomination of the stack
      while (denominations.top > m) denominations.pop
      change(m - denominations.top, denominations, iteration + 1)
    }
  }
}
