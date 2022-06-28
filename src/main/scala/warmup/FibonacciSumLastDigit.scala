package warmup

import java.util.Scanner
import scala.annotation.tailrec

object FibonacciSumLastDigit {

  /*
    Fibonacci:
    0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, ...

    Fibonacci Sum:
    0, 1, 2, 4, 7, 12, 20, 33, 54, ...

    So it looks like:
    Σ(F(n)) = F(n+2) - 1
  */

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextLong()
    println(fibonacciSumLastDigit(n))
  }

  def fibonacciSumLastDigit(n: Long) = {

    val sum = fibonacciModulo10(n + 2) - 1
    (10 + sum) % 10 // always a positive number
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
