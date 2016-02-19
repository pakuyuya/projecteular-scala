package eular.p002

import util.Measure

object Main {
  def main(args : Array[String]) = {
    Measure.ms(answer1)
  }

  def answer1() = {
    val fs: Stream[Int] = fibFrom(1, 1)

    val answer = fs.takeWhile( x => x < 4000000 )
      .filter(_ % 2 == 0)
      .sum

    println(answer)
  }

  def fibFrom(a: Int, b:Int): Stream[Int] = a #:: fibFrom(b, a + b)
}