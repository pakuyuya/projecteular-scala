package eular.p001

import util.Measure

object Main {

  def main(args: Array[String]) : Unit = {
    println("answer1")
    Measure.ms(answer1)

    println("-------------------")
    println("answer2")
    Measure.ms(answer2)
  }

  def answer1() = {
    val answer = List.range(1, 1000)
      .filter(n => (n%3 == 0) || (n%5 == 0))
      .reduce((total, n) => total + n)

    println(s"""answer:${ answer }""")
  }

  def answer2() = {
    val answer = List.range(1, 1000)
      .filter(n => (n%3 == 0) || (n%5 == 0))
      .sum

    println(s"""answer:${ answer }""")
  }
}