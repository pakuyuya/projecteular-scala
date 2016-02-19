package eular.p012

import util.Measure

object Main {
  def main(args: Array[String]) = {
    Measure.ms(() => println("problem 12"))

    println("answer1 running...");
    Measure.ms(answer1)

    println("----------------------------------");
    println("godSaid ...");
    Measure.ms(godSaid)
  }

  /**
   * 素因数分解＋約数の数の公式
   */
  def answer1() = {
    def trinum(i:Int, n:Int):Stream[Int] = n #:: trinum(i+1, n+i+1)

    def factors(n: Int): List[Int] =
      (2 to Math.sqrt(n).toInt)
        .find(n % _ == 0)
        .fold(List(n))(i => i:: factors(n / i))

    def divisorNum(facts:Seq[Int]) = {
      facts.distinct
        .map(i => facts.filter(_ == i).size + 1)
        .product
    }

    val answer =
      trinum(1, 1)
        .find(i => divisorNum(factors(i)) > 500)
        .get

    println(s"answer: ${ answer }")
  }

  def godSaid() {
    // あれ、理屈が分からない。
    // ある自然数の平方根 は、約数の丁度中位置になる性質がある？？
    // でも、平方根の数のときに約数を２個計上しているのはバグだな

    lazy val ts: Stream[Int] =
      0 #:: ts.zipWithIndex.map(p => p._1 + p._2 + 1)

    def p(t: Int) = Range(1, Int.MaxValue)
      .takeWhile(n => n * n <= t)
      .foldLeft(0)((s, n) => if (t % n == 0) s + 2 else s)

    val answer = ts.find(p(_) > 500).get

    println(s"answer: ${ answer }")
  }
}