package com.zx80live.mod.fastcards.util

import java.io.{File, FileOutputStream, OutputStream}

import com.zx80live.mod.fastcards.OldCard
import resource._

object CardsWriter {

  def write(c: OldCard)(implicit out: OutputStream): Unit = {


    val str = c.value + c.transcript.map(t => s"[$t]").getOrElse("") + ":" + c.kind.getOrElse("") + ":" + c.translations.mkString("|") +
      c.examples.map { e =>
        "\n\t* " + e.text + (if (e.translations.nonEmpty) {
          ":" + e.translations.map(_.trim).mkString("|")
        })
      }.mkString("") +
      "\n\n"

    out.write(str.getBytes)
  }

  def write(xs: List[OldCard], file: File): Unit = {
    for (out <- managed(new FileOutputStream(file))) {
      implicit val os = out

      xs.map(write)
      os.flush()
    }
  }
}
