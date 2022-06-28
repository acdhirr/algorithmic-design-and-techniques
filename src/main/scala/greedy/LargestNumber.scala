package greedy

import java.util.Scanner

object LargestNumber {

  case class LexicalInt(value: Int) extends Ordered[LexicalInt] {

    override def compare(that: LexicalInt): Int = {
      val s1 = this.value.toString
      val s2 = that.value.toString
      if (s1.length == s2.length) s2.compareTo(s1)
      else s2.concat(s1).compareTo(s1.concat(s2))
    }

    override def toString() = value.toString
  }

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val n = s.nextInt()
    val a = new Array[LexicalInt](n)
    for (i <- 0 until n) a(i) = LexicalInt(s.nextInt())

    a.sorted.foreach(print)
  }

}
