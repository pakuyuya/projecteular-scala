package eular.p010

import util.Measure
import Math.sqrt

object Main {


  def main(args: Array[String]) = {
    Measure.ms(() => println("problem 10"))

    println("answer1 running...");
    Measure.ms(answer1)

    println("----------------------------------");
    println("answer2 running...");
    Measure.ms(answer2)

  }

  def answer1() = {

    lazy val ps: Stream[Int]
      = 2 #:: Stream
        .from(3)
         .filter(i => ps.takeWhile(j => j * j <= i).forall(i % _ > 0))

    val answer = ps.takeWhile(_ <= 2000000).foldLeft(0L)(_ + _)

    println(s"answer: ${ answer }")
  }

  def answer2() = {

    def isPrime(n: Int): Boolean = {
      if (n == 2) return true
      for (i:Int <- 3 to sqrt(n).toInt by 2) {
        if (n % i == 0) return false
      }
      return true
    }

    def nextPrime(n: Int): Stream[Int] = {
      var i = if(n % 2 == 0) n + 1 else n + 2;
      while (!isPrime(i)) i += 2
      return n #:: nextPrime(i)
    }

   val answer = nextPrime(2).takeWhile(_ <= 2000000).foldLeft(0L)(_ + _)
    println(s"answer: ${ answer }")
  }
}