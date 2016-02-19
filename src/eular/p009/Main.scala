package eular.p009

import util.Measure

object Main {

  def main(args: Array[String]) = {
    Measure.ms(() => println("problem 9"))

    println("answer1");
    Measure.ms(answer1)

    println("----------------------------------");
    println("god said...");
    Measure.ms(godSaid)
  }

  /**
   * (a, b, c)の総当りから条件を絞るアプローチ
   */
  def answer1() = {
    val tuple =
      (1 to 998)
        .flatMap(a => {
          (1 to 999 - a)
            .map( b => (a, b, 1000 - a - b))
        }
      )
      .filter { case(a,b,c) => a < b && b < c }
      .filter { case(a,b,c) => a*a + b*b == c*c }
       .last

    val answer = tuple match { case(a, b, c) => a * b * c }

    println(s"answer : ${ answer }")
  }


  def godSaid() = {

    // for ... だと ... !?

    val limit = (1 to 1000).find(n => n + math.sqrt(n) >= 1000).get

    val rs
      = for (b <- 2 until limit; a <- 1 until b; c = 1000 - a - b
                if a * a + b * b == c * c) yield a * b * c

    val answer = rs.head

    println(s"answer : ${ answer }")
  }
}