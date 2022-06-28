package warmup

import java.util.Scanner

object Fibonacci {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()
    println(fibonacci(n))
  }

  def fibonacci(n: Integer) = {

    val ar = new Array[Long](n + 1)
    ar(0) = 0L
    if (n > 0) ar(1) = 1L
    for (i <- 2 to n) {
      ar(i) = ar(i - 1) + ar(i - 2)
    }
    ar(n)
  }

}
