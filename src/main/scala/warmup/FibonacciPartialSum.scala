package warmup

import java.util.Scanner
import scala.annotation.tailrec

object FibonacciPartialSum {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val m = s.nextLong()
    val n = s.nextLong()
    println(fibonacciPartialSum(m, n))
  }

  def fibonacciPartialSum(m: Long, n: Long) = {

    /*
      Σ(F(m)) = F(m+2) - 1
      Σ(F(n)) = F(n+2) - 1

      Σ(F(m->n)) = F(n+2) - 1 - (F(m+1) - 1)  including, so m+1
                 = F(n+2) - F(m+1)
    */

    (10 + fibonacciModulo10(n + 2) - fibonacciModulo10(m + 1)) % 10
  }

  def fibonacciModulo10(n: Long): Long = {

    @tailrec // reuse this code from 'FibonacciLastDigit'
    def inner(a: Long, b: Long, n: Long): Long = {
      val ld = (a + b) % 10
      if (n > 0) inner(b, ld, n - 1) // recursion: shrink s to zero
      else ld
    }

    val np = n % 60 // pisano period for modulo 10 is 60

    if (np == 0) 0
    else inner(0, 1, np - 2) // np-2 because we already have used the first two values (0,1)
  }

}
