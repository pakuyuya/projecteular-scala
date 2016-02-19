package eular.p006

import util.Measure
import Math.pow
import Math.abs

object Main {

  def main(args: Array[String]) = {
    Measure.ms(() => println("problem 6"))

    println("answer1");
    Measure.ms(answer1)
  }

  /**
   * シンプルに
   */
  def answer1() = {

    val squaresum = (1 to 100).map(i => i * i).sum
    val sum = (1 to 100).sum

    val answer = abs(squaresum - sum * sum)

    println(s"answer: ${ answer }")
  }

  def godsaid() = {
    // なるほどもなてぃっく。
    def square = (n: Int) => n * n
    val answer = square((1 to 100).sum) - (1 to 100).map(square).sum

    println(s"answer: ${ answer }")
  }

}