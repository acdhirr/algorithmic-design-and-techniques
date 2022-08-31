package divideandconquer

import java.util.Scanner

/*
TEST

11
4 4
-2 -2
-3 -4
-1 3
2 3
-4 0
1 1
-1 -1
3 -1
-4 2
-2 4

-> 1.414213

2
1000000000 -1000000000
-1000000000 1000000000

3
0 0
1 1
0 0

*/

object Closest {

  case class Point(x: Long, y: Long) {

    // distances are relative so we can do with the square distance, which is an Int
    def squareDistance(that: Point): Long = square(this.x - that.x) + square(this.y - that.y)
    override def toString() = s"($x, $y)"
  }

  def square(v: Long): Long = v*v

  def split(points: Array[Point]): (Array[Point], Array[Point]) =
    points.splitAt(points.length / 2)

  def borderPoints(points: Array[Point], x: Long, dist: Long) =
    points.filter(p => square(p.x - x) < dist)

  // returns quadratic distance!
  def minSquareDistance(points: Array[Point]): Long = {

    // inner function assumes points to be sorted by x
    def _dst(points: Array[Point]): Long = {

      // keep dividing when more then 4 points are involved
      if (points.length > 4) {
        val (pointsL, pointsR) = split(points)
        val minDst = Math.min( _dst(pointsL), _dst(pointsR) )
        val strip = borderPoints(points, pointsL(pointsL.length-1).x, minDst)
        // maybe a pair spanning the border strip has a smaller distance
        Math.min( minDst, borderMinimum(strip) )
      }
      // no more further dividing
      else {
        val distances: Array[Long] = for {
          p1 <- points
          p2 <- points
          if p1 ne p2
        } yield p1.squareDistance(p2)
        if (distances.isEmpty) 0 else distances.min
      }
    }

    // make sure the array is sorted by x before calling recursive function
    _dst( points.sortBy(_.x) )
  }

  def borderMinimum(points: Array[Point]): Long = {

    val pts = points.sortBy(_.y)
    val l = pts.length

    val distances = for {
      i <- 0 until l
      j <- i until i+7
      if j < l-1
      if pts(i) ne pts(j)
    } yield pts(i).squareDistance(pts(j))

    if (distances.isEmpty) Long.MaxValue else distances.min
  }


  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()
    val points = new Array[Point](n)
    for (i <- 0 until n) points(i) = Point(s.nextLong(), s.nextLong())

    println( Math.sqrt( minSquareDistance(points) ) )
  }

}

