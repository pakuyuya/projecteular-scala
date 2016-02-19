package util

import scala.concurrent._
import scala.concurrent.duration.Duration
import ExecutionContext.Implicits.global

object Measure {
  def ms[A](f : => A) {
    val start = System.currentTimeMillis()
    val task: Future[Unit] = Future {
      f
      val end = System.currentTimeMillis()
      println(s"""${ end - start }ms""")

    }

    task onFailure {
      case t => {
        println(t.getMessage + t.getStackTraceString)
      }
    }

    Await.ready(task, Duration.Inf)

  }
}