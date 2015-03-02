package com.zx80live.mod.fastcards.util

object LoanPattern {

  import scala.language.reflectiveCalls
  import scala.util.control.Exception._

  def using[R <: {def close()}, A](resource: R)(f: R => A): A = {
    try {
      f(resource)
    } finally {
      ignoring(classOf[Throwable]) apply {
        resource.close()
      }
    }
  }
}

