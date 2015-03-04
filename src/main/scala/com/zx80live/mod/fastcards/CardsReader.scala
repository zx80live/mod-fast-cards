package com.zx80live.mod.fastcards

import java.io.File

import com.zx80live.mod.fastcards.util.NIOUtils

import scala.language.implicitConversions

object CardsReader {

  implicit def string2Option(str: String): Option[String] = str.trim match {
    case s: String if s.length > 0 => Some(s)
    case _ => None
  }

  def read(filename: String): List[Card] = {
    val wordPattern = """^([\w\s]+)\:([\s\w]*)\:([\s\|\wа-яА-Я]*)$""".r
    val examplePattern = """^\s*\*([\w\s]+)\s*\:*([\s\|\wа-яА-Я]*)\s*$""".r
    var card: Option[Card] = None
    var xs: List[Card] = Nil

    NIOUtils.readDelimitedStrings(new File(filename), '\n') {
      case wordPattern(value, kind, trans) =>
        if (card.isDefined)
          xs = card.get :: xs


        card = Some(Card(value, kind: Option[String], trans.split("\\|").map(_.trim).toList))

      case examplePattern(text, trans) =>
        val e = Example(text, trans.split("\\|").map(_.trim).toList)
        card match {
          case Some(c) =>
            card = Some(c.copy(examples = e :: c.examples))
          case _ => Unit
        }
      case _ => Unit
    }
    xs.reverse
  }
}
