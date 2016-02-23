package eular.p014

import util.Measure

object Main {

  def main(args : Array[String]) = {
    Measure.ms(() => println("problem 13"))

    println("answer1 running...");
    Measure.ms(answer1)

    println("----------------------------------");
    println("godSaid ...");
    Measure.ms(godSaid)
  }

  /**
   * クソ重くて終わらなかった。
   * Streamが重いのか、コレクション操作でまずったか。
   */
  def answer1() {
    def collatz(n:Int): Stream[Int] = {
      if (n%2 == 0) (n/2) #:: collatz(n/2)
      else          (3*n + 1) #:: collatz(3*n + 1)
    }

    val answer =
      (1 until 1000000).view
        .map(i => (i, collatz(i).takeWhile(_ != 1).size))
        .max(Ordering[Int].on[(_,Int)](_._2))
        ._1

    println(answer)
  }

  def godSaid() {
    def from(n: Long, c: Int = 0): Int = if (n == 1) c + 1 else
      from(if (n % 2 == 0) n / 2 else 3 * n + 1, c + 1)

    val r = (1 until 1000000).view.map(n => (n, from(n)))
      .reduceLeft((a, b) => if (a._2 > b._2) a else b)._1

    println(r) // 1 s
  }
}