package eular.p003

import util.Measure
import Math.sqrt

object Main {
  def main(args : Array[String]) = {

    Measure.ms(()=> println("init"))

    println("answer1")
    Measure.ms(answer1)

    println("-------------------")
    println("god said")
    Measure.ms(godSaid)
  }


  def godSaid() = {
    def factors(n: Long): List[Long] =
      (2 to Math.sqrt(n).toInt)
        .find(n % _ == 0)
        .fold(List(n))(i => i.toLong:: factors(n / i))


    val answer = factors(600851475143L).last

    println(s"""answer: ${ answer }""")
  }
  def answer1() = {

    var base:Long = 600851475143L

    def nextFactors(i:Int, n: Long): List[Long] =
      (i to Math.sqrt(n).toInt)
        .find(n % _ == 0)
        .fold(List(n))(i => i.toLong:: nextFactors(i, n / i))

    var answer = nextFactors(2, base).last;

    println(s"""answer: ${ answer }""")
  }
}