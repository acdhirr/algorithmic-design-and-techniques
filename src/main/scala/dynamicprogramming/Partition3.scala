package dynamicprogramming

import java.util.Scanner

object Partition3 {

  def main(args: Array[String]): Unit = {

    val scan = new Scanner(System.in)

    val n = scan.nextInt()
    val values = new Array[Int](n)
    for (i <- 0 until n) values(i) = scan.nextInt()

    println( if (partition3(values)) 1 else 0 )

  }

  /**
   * Check whether values can be partitioned in 3 equal sum parts
   * @param values
   * @return
   */
  def partition3(values: Array[Int]): Boolean = {

    val sum = values.sum
    val n = values.length
    val vals = 0 +: values // prepend an empty value to make values 1-based

    if (sum % 3 != 0) false

    else {
      val third = sum / 3
      // 3d table will be populated with false values
      val cube = Array.ofDim[Boolean](n + 1, third + 1, third + 1)
      cube(0)(0)(0) = true

      for (i <- 1 to n)
        for (s1 <- 0 to third)
          for (s2 <- 0 to third) {
            cube(i)(s1)(s2) =
            if (vals(i) <= s1) cube(i-1)(s1)(s2) || cube(i-1)(s1-vals(i))(s2)
            else if (vals(i) <= s2) cube(i-1)(s1)(s2) || cube(i-1)(s1)(s2-vals(i))
            else cube(i-1)(s1)(s2)
          }

      // right lower rear corner contains answer
      cube(n)(third)(third)
    }
  }

}

/*
Some tests:

1
30
-> 0

4
1 2 3 3
-> 1

13
1 2 3 4 5 5 7 7 8 10 12 19 25
-> 1

*/
