package com.zx80live.mod.fastcards.util

import java.io.{File, FileOutputStream, OutputStream}

import com.zx80live.mod.fastcards.Card
import resource._

object CardsWriter {

  def write(c: Card)(implicit out: OutputStream): Unit = {


    val str = c.data.value + c.data.transcript.map(t => s"[$t]").getOrElse("") + ":" + c.data.kind.getOrElse("") + ":" + c.data.translations.mkString("|") +
      c.data.examples.map { e =>
        "\n\t* " + e.text + (if (e.translations.nonEmpty) {
          ":" + e.translations.map(_.trim).mkString("|")
        })
      }.mkString("") +
      "\n\n"

    out.write(str.getBytes)
  }

  def write(xs: List[Card], file: File): Unit = {
    for (out <- managed(new FileOutputStream(file))) {
      implicit val os = out

      xs.map(write)
      os.flush()
    }
  }
}
