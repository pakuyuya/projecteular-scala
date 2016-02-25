package eular.p018

import util.Measure
import java.math.BigDecimal
import Math.max

object Main {

  def main(args : Array[String]) = {
    Measure.ms(() => println("problem 18"))

    println("answer1 running...");
    Measure.ms(answer1)

    println("----------------------------------");
    println("godSaid ...");
    Measure.ms(godSaid)
  }

  val sdata =
  """75
	95 64
	17 47 82
	18 35 87 10
	20 04 82 47 65
	19 01 23 75 03 34
	88 02 77 73 07 63 67
	99 65 04 28 06 16 70 92
	41 41 26 56 83 40 80 70 33
	41 48 72 33 47 32 37 16 94 29
	53 71 44 65 25 43 91 52 97 51 14
	70 11 33 28 77 73 17 78 39 68 17 57
	91 71 52 38 17 14 91 43 58 50 27 29 48
	63 66 04 68 89 53 67 30 73 16 69 87 40 31
	04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"""

  /**
   * マジメに
   */
  def answer1() {
  	val datas = sdata.trim.split("\\s+").view.map(_.toInt).zipWithIndex

  	val answer =
  		(14 to 1 by -1)
	  		.foldLeft(datas)((d, i) => {
	  			val lidx = (1 until i).sum
	  			val uidx = (1 until i + 1).sum
	  			d.map({
	  				case (n, idx) if(lidx<=idx && idx<uidx) => (n + max(d(idx+i)._1,d(idx+i+1)._1), idx)
	  				case t  => t
	  			})
	  		}).head._1

    println(s"answer: ${ answer }")
  }

  def godSaid() {
    // ほほう。複数の行をまとめるには、zipが便利だな。

    val s = sdata

    val grid = s.trim.split("\n").map(_.split("\\s+").map(_.toInt))

    def f(rows: Array[Array[Int]], bottom: Seq[Int]): Int = {
      val ms = bottom.zip(bottom.tail).map(p => p._1 max p._2)
      val ss = rows.last.zip(ms).map(p => p._1 + p._2)
      if (ss.length == 1) ss.head else f(rows.init, ss)
    }

    val r = f(grid.init, grid.last)

    println(r)
  }
}