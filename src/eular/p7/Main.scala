package eular.p7

import util.Measure
import Math.sqrt

object Main {

  def main(args: Array[String]) = {
    Measure.ms(() => println("problem 4"))

    println("answer1");
    Measure.ms(answer1)

    println("----------------------------------");
    println("god said...");
    Measure.ms(godSaid)
  }

  def answer1() = {

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

    val answer = nextPrime(2).take(10001).last

    println(s"answer: ${ answer }")
  }

  def godSaid() {
    // Streamは概念

    lazy val ps: Stream[Int]
      = 2 #:: Stream
        .from(3)
         .filter(i => ps.takeWhile(j => j * j <= i).forall(i % _ > 0))

    val answer = ps(10000)
    println(s"answer: ${ answer }")
  }
}