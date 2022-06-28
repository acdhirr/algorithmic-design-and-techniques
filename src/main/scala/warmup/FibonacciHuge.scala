package warmup

import java.util.Scanner
import scala.annotation.tailrec

object FibonacciHuge {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextLong() // 1 <= n <= 10¹⁴
    val mod = s.nextInt() // 1 <= m <= 10³
    println(fibonacciModulo(n, mod))
  }

  def fibonacciModulo(n: Long, modulo: Int): Int = {

    @tailrec // reuse this code from 'FibonacciLastDigit'
    def inner(a: Integer, b: Integer, n: Long): Integer = {
      val ld = if (modulo > 1) (a + b) % modulo else a + b
      if (n > 0) inner(b, ld, n - 1) // recursion: shrink s to zero
      else ld
    }

    val np = n % pisanoPeriod(modulo)

    if (np == 0) 0
    else inner(0, 1, np - 2) // np-2 because we already have used the first two values (0,1)
  }

  def pisanoPeriod(modulo: Int) = {

    // A closure :-)
    @tailrec
    def inner(a: Int, b: Int, period: Int): Int = {
      // repeat until the start marker (0,1) appears again:
      // then we have found a full period
      if (period > 0 && (a, b) == (0, 1)) period
      else inner(b, (a + b) % modulo, period + 1)
    }

    if (modulo < 2) modulo
    else inner(0, 1, 0)
  }

}
