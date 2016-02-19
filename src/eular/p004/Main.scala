package eular.p004

import util.Measure

object Main {
  def main(args: Array[String]) = {
    Measure.ms(() => println("problem 4"))

    Measure.ms(answer1)

    Measure.ms(godSaid)
  }

  def answer1() = {

    def isPalindromic(n:Int):Boolean = {
      val str = n.toString()

      val invalid_char = str.zipWithIndex.find {
        case(c:Char, i:Int) =>
          c != str.charAt(str.length()-1-i)
      }

      invalid_char match {
        case Some(_) => false
        case None    => true
      }
    }

    // ｶﾐｻﾏに教えてもらいました
    val answer = (100 to 999)
      .flatMap(i => (i to 999).map(_ * i))
      .filter(isPalindromic(_))
      .max

    println(s"""answer: ${ answer }""");
  }

  def godSaid() = {
    // ﾎｹﾞｪ
    val answer = (100 to 999)
      .flatMap(i => (i to 999).map(_ * i))
      .filter(i => i.toString() == i.toString().reverse)
      .max

      println(s"""answer: ${ answer }""");
}
}