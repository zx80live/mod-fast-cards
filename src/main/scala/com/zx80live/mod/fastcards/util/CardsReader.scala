package com.zx80live.mod.fastcards.util

import java.io.File

import com.zx80live.mod.fastcards.{Data, Example, Card}

import scala.language.implicitConversions

object CardsReader {

  implicit def string2Option(str: String): Option[String] = str.trim match {
    case s: String if s.length > 0 => Some(s)
    case _ => None
  }

  def read(file: File, verboseParse: Boolean = false): List[Card] = {
    val textContent = """\w\s|'\(\)\=\-а-яА-Я,\.`"""
    val wordPattern = ("""^([""" + textContent + """]+)(\s*\[[\w\sа-яА-Я]+\]\s*)*\:([\s\w]*)\:([""" + textContent + """]*)$""").r
    val examplePattern = ("""^\s*\*([""" + textContent + """]+)\s*\:*([""" + textContent + """]*)\s*$""").r
    var card: Option[Card] = None
    var xs: List[Card] = Nil

    NIOUtils.readDelimitedStrings(file, '\n') {
      case wordPattern(value, transcript, kind, trans) =>
        if (card.isDefined)
          xs = card.get :: xs

        val t = if (transcript != null) Some(transcript.trim.tail.take(transcript.trim.length - 2)) else None

        card = Some(Card(Data(value.trim, kind: Option[String], trans.split("\\|").map(_.trim).toList, Nil, t)))

      case examplePattern(text, trans) =>
        val e = Example(text.trim, trans.split("\\|").map(_.trim).collect { case s if !s.isEmpty => s }.toList)
        card match {
          case Some(c) =>
            card = Some(Card(c.data.copy(examples = e :: c.data.examples))) //Some(c.copy(examples = e :: c.examples))
          case _ => Unit
        }
      case text:String =>
        if(verboseParse) {
          if(text.trim.length > 0)
            println(text)
        }
        Unit
    }
    if (card.isDefined)
      xs = card.get :: xs

    xs.reverse
  }
}
