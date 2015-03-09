package com.zx80live.mod.fastcards

import java.io.{FileOutputStream, FileInputStream, File, OutputStream}

import resource._

object CardsWriter {

  def write(c: Card)(implicit out: OutputStream): Unit = {


    val str = c.value + c.transcript.map(t => s"[$t]").getOrElse("") + ":" + c.kind.getOrElse("") + ":" + c.translations.mkString("|") +
      c.examples.map { e =>
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
