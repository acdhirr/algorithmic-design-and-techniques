package warmup

import java.util.Scanner
import scala.annotation.tailrec

object FibonacciLastDigit {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()
    println(fibonacciLastDigitStep(0, 1, n - 2)) // n-2 because we already have used the first two values (0,1)
  }

  @tailrec
  def fibonacciLastDigitStep(a: Integer, b: Integer, n: Integer): Integer = {

    // use modulo 10 - it's only the last digit that counts
    val ld = (a + b) % 10
    if (n > 0) fibonacciLastDigitStep(b, ld, n - 1) // recursion: shrink n to zero
    else ld
  }

}
