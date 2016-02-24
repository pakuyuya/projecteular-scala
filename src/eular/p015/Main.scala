package eular.p015

import util.Measure
import java.math.BigDecimal

object Main {

  def main(args : Array[String]) = {
    Measure.ms(() => println("problem 13"))

//    println("answer1 running...");
//    Measure.ms(answer1)
    println("answer1 running...");
    Measure.ms(answer1)
    println("answer2 running...");
    Measure.ms(answer2)

    println("----------------------------------");
    println("godSaid ...");
    Measure.ms(godSaid)
  }

  /**
   * 組み合わせの個数の公式
   */
  def answer1() {
    val molecule =
      (21 to 40)
        .foldLeft(BigDecimal.ONE)((n, i) => n.multiply(BigDecimal.valueOf(i)))

    val answer = (2 to 20)
      .foldLeft(molecule)((n,i) => n.divide(BigDecimal.valueOf(i)))

    println(answer)
  }
  /**
   * ちょっとひねって。immutableじゃないけど。。<br>
   * <br>
   * 頂点単位で考える。<br>
   * ・・・<br>
   * ・・・<br>
   * ・・・<br>
   * 問題の図を180度ひっくり返して、左上をゴールとする。<br>
   * 最短ルートは、「１つ上」または「１つ左」へ行くの２通り<br>
   * 　→「その頂点のパターン数」＝「１つ上のパターン数」＋「１つ左のパターン数」。<br>
   * 例外として、上辺、左辺は 1パターン とする。<br>
   * <br>
   * とすると、各頂点ごとの、ゴールまでのパターン数は<br>
   * 1 1 1<br>
   * 1 2 3<br>
   * 1 3 6<br>
   * となる。<br>
   */
  def answer2() {
    val list = scala.collection.mutable.ArrayBuffer.fill(21*21)(1L)

    for (x <- 1 to 20; y <- 1 to 20) {
      val i = x + y*21
      list(i) = list(i-1) + list(i-21)
    }
    val answer = list.last


    println(s"answer: ${ answer }")
  }

  def godSaid() {
    // answer2() を超エレガントにやってる。
    // 確かに、左から右への増分をcarryしている構造だ。超数学的！！

    def f(row: Seq[Long], c: Int): Long =
      if (c == 0) row.last else f(row.scan(0L)(_ + _), c - 1)

    def r(n: Int) = f(Seq.fill(n + 1)(1L), n)

    println(r(20));
  }
}