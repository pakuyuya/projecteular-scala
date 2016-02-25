package eular.p017

import util.Measure
import java.math.BigDecimal

object Main {

  def main(args : Array[String]) = {
    Measure.ms(() => println("problem 17"))

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
    def makeWords(i:Int) = {

      val words0and1to19
         = List("",        "one",     "two",       "three",    "four",
                "five",    "six",     "seven",     "eight",    "nine",
                "ten",     "eleven",  "twenteen",  "thirteen", "fourteen",
                "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" )
      val words20to90by10
        = List("", "", "twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninty")


      ((i / 1000) % 10 match {
        case n if(n>0) => words0and1to19(n) + " thousand "
        case _         => ""
      }) +
        ((i / 100) % 10 match {
          case n if(n>0) => words0and1to19(n) + " hundred "
          case _         => ""
        }) +
          (i match {
            case n if(n>100 && n%100>0) => "and "
            case _                      => ""
          }) +
            (i % 100 match {
              case n if(n == 0) => ""
              case n if(n <=19) => words0and1to19(n)
              case n          => words20to90by10(n/10) + " " + words0and1to19(n%10)
            })
            .trim
    }

    val answer =
      (1 to 1000)
        .map(makeWords(_).replace(" ", "").length())
        .sum

    println(s"answer: ${ answer }")
  }

  def godSaid() {
    val units = Array(0, 3, 3, 5, 4, 4, 3, 5, 5, 4, 3, 6, 6, 8, 8, 7,
      7, 9, 8, 8)

    val tens = Array(0, 0, 6, 6, 5, 5, 5, 7, 6, 6)

    lazy val name: Int => Int = {
      case n if n < 20 => units(n)
      case n if n < 100 =>
        tens(n / 10) + (if (n % 10 > 0) units(n % 10) else 0)
      case n if n < 1000 =>
        name(n / 100) + 7 + (if (n % 100 > 0) 3 + name(n % 100) else 0)
      case 1000 => 11
    }

    val r = (1 to 1000).map(name).sum
    println(r)
  }
}