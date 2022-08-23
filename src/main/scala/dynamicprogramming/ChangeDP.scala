package dynamicprogramming

import java.util.Scanner
import scala.math.max

object ChangeDP {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val m = s.nextInt()
    val denominations = List(1, 3, 4) // types of coins (values in cents)

    println(change(m, denominations))
  }

  def change(m: Int, denominations: List[Int]): Int = {

    val mem = Array.fill(m+1)(-1) // initialize with -1
    mem(0) = 0

    def inner(n: Int): Int =
      if (mem(n) > -1) mem(n)
      else {
        // n-d must never be less than 0!
        mem(n) = denominations.map(d => inner(max(0, n - d)) + 1).min
        mem(n)
      }

    inner(m)
  }

}
