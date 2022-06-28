package challenges

object MaxPairwiseProduct {

  def main(args: Array[String]) = {
    val s = new FastScanner(System.in)
    val n = s.nextInt
    val numbers = new Array[Int](n)
    for (i <- 0 until n) numbers(i) = s.nextInt
    // numbers.foreach(n => println(n))
    println(getMaxPairwiseProduct(numbers))
  }

  def getMaxPairwiseProduct(numbers: Array[Int]): Long = {

    val p: (Long, Long) = numbers.foldLeft((0L, 0L))((cur, n) => {
      // Change pair when necessary
      val first = cur._2
      val second = cur._1
      if (n > first) (first, n)
      else if (n > second) (n, first)
      else (second, first)
    })

    p._1 * p._2
  }

  import java.io.{BufferedReader, IOException, InputStream, InputStreamReader}
  import java.util.StringTokenizer

  class FastScanner(val stream: InputStream) {

    var br: BufferedReader = null
    var st: StringTokenizer = null

    try br = new BufferedReader(new InputStreamReader(stream))
    catch {
      case e: Exception => e.printStackTrace()
    }

    def next: String = {
      while (st == null || !st.hasMoreTokens)
        try st = new StringTokenizer(br.readLine())
        catch {
          case e: IOException => e.printStackTrace()
        }

      st.nextToken
    }

    def nextInt: Int = next.toInt
  }

}
