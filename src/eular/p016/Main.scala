package eular.p016

import util.Measure
import java.math.BigDecimal

object Main {

  def main(args : Array[String]) = {
    Measure.ms(() => println("problem 16"))

    println("answer1 running...");
    Measure.ms(answer1)

    println("----------------------------------");
    println("godSaid ...");
    Measure.ms(godSaid)
  }

  /**
   * マジメに
   */
  def answer1() {
    def doubleup(l:List[Int]) = {
      val doubleWithCarry = l.scanLeft((0,0))((pt, i) => ((pt._2 + i*2)%10, if(i>=5) 1 else 0)).tail
      doubleWithCarry.map(_._1) ::: (if (doubleWithCarry.last._2 > 0) List(1) else List.empty[Int])
    }

    val answer = (1 to 1000)
      .foldLeft(List(1))((z, n) => doubleup(z))
        .sum

    println(s"answer: ${ answer }")
  }

  def godSaid() {
    val r = BigInt(2).pow(1000).toString.view.map(_.asDigit).sum
    println(r)
  }
}