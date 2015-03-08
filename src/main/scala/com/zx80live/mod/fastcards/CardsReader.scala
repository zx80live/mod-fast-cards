package com.zx80live.mod.fastcards

import java.io.File

import com.zx80live.mod.fastcards.util.NIOUtils

import scala.language.implicitConversions
import scala.util.matching.Regex

object CardsReader {

  implicit def string2Option(str: String): Option[String] = str.trim match {
    case s: String if s.length > 0 => Some(s)
    case _ => None
  }

  def read(filename: String): List[Card] = {
    val textContent = """\w\s|'\(\)\=\-а-яА-Я,\."""
    val wordPattern = ("""^([""" + textContent + """]+)(\s*\[[\w\sа-яА-Я]+\]\s*)*\:([\s\w]*)\:([""" + textContent + """]*)$""").r
    val examplePattern = ("""^\s*\*([""" + textContent + """]+)\s*\:*([""" + textContent + """]*)\s*$""").r
    var card: Option[Card] = None
    var xs: List[Card] = Nil

    NIOUtils.readDelimitedStrings(new File(filename), '\n') {
      case wordPattern(value, transcript, kind, trans) =>
        if (card.isDefined)
          xs = card.get :: xs

        val t = if (transcript != null) Some(transcript.trim.tail.take(transcript.trim.length - 2)) else None

        card = Some(Card(value.trim, kind: Option[String], trans.split("\\|").map(_.trim).toList, Nil, t))

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
