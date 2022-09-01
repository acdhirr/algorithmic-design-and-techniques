package dynamicprogramming

import java.util.Scanner

object LCS3 {

  def main(args: Array[String]): Unit = {

    val scan = new Scanner(System.in)

    val n1 = scan.nextInt()
    val s = new Array[Int](n1)
    for (i <- 0 until n1) s(i) = scan.nextInt()

    val n2 = scan.nextInt()
    val t = new Array[Int](n2)
    for (i <- 0 until n2) t(i) = scan.nextInt()

    val n3 = scan.nextInt()
    val u = new Array[Int](n3)
    for (i <- 0 until n3) u(i) = scan.nextInt()

    /*

    3
    1 2 3
    3
    2 1 3
    3
    1 3 5

    -> 2

    5
    8 3 2 1 7
    7
    8 2 1 3 8 10 7
    6
    6 8 3 1 4 7

    -> 3

    */

    println(lcs3(s,t,u))

  }

  // Use a 3d-table (cube) this time (no recursion)
  def lcs3(s: Array[Int], t: Array[Int], u: Array[Int]): Int = {

    // cells will be pre-populated with zeros
    val cube = Array.ofDim[Int](s.length+1, t.length+1, u.length+1)

    for (i <- 1 to s.length)
      for (j <- 1 to t.length)
        for (k <- 1 to u.length) {

          val biggestNeighbour =
            Set( cube(i-1)(j)(k), cube(i)(j-1)(k), cube(i)(j)(k-1) ).max

          cube(i)(j)(k) =
            if( s(i-1) == t(j-1) && t(j-1) == u(k-1) ) Set( biggestNeighbour, cube(i-1)(j-1)(k-1)+1 ).max
            else biggestNeighbour
        }

    cube(s.length)(t.length)(u.length)
  }

}
