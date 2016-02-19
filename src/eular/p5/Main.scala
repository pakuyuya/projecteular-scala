package eular.p5

import util.Measure

object Main {
  def main(args: Array[String]) = {
    Measure.ms(() => println("problem 4"))

    println("answer1");
    Measure.ms(answer1)

    println("----------------------------------");
    println("god said...");
    Measure.ms(godSaid)
  }

  /**
   * ある自然数の集合に対する最小公倍数は、
   * 各自然数を素因数分解して、素因数ごとに最も累乗数が多かったものを集め、累乗したものを掛け合わせると求まることを利用。
   *
   * 例えば、4（2^「2」）と6（2^1・3^「1」）の最小公倍数は 12。（2^「2」・3^「1」）
   */
  def answer1() = {

    // 素因数分解する関数
    def factors(n: Int): List[Int] =
      (2 to Math.sqrt(n).toInt)
        .find(n % _ == 0)
        .fold(List(n))(i => i:: factors(n / i))

    // 1～20を素因数分解したリスト
    val factorsList = (1 to 20).map(factors(_))

    val answer =
      (1 to 20) // 素因数候補
        .map(i => {
          val maxcnt = factorsList.map{ facts => facts.filter(_ == i).size }.max // 指定した素因数の最大出現数を取得
          (1 to maxcnt).fold(1)((z, n) => z * i) // 回数分だけかける
        })
        // (素因数^最大出現回数) のリストが完成
        .product // 全部かける

    println(s"answer: ${ answer }")
  }

  def godSaid() = {
    // ごり押しィ！
    val answer = Range(20, Int.MaxValue)
      .find(n => Range(2, 21).forall(n % _ == 0)).get

    println(s"answer: ${ answer }")
  }
}