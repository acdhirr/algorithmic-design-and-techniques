package warmup

import java.util.Scanner
import scala.annotation.tailrec

object GCD {

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val a = s.nextInt()
    val b = s.nextInt()
    println(euclidesGcd(a, b))
  }

  @tailrec
  def euclidesGcd(a: Long, b: Long): Long =
    if (b == 0) a
    else euclidesGcd(b, a % b)

}
