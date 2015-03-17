package com.zx80live.mod.fastcards

import scala.tools.jline.console.{ConsoleReader => R}

object ExamController extends ExamExtensions with ExamFSM with ArgumentParser {

  import com.zx80.mod.util.console.ConsoleCSS._

  object Renderers {

    def renderStartExam(badFilePrefixOpt: Option[String]): String =
      "\nstart exam".attr(Format.Bold | Foreground.color(70)) + badFilePrefixOpt.map(_ => "").getOrElse(" " + "repeat bad".attr(Foreground.color(22))) + "\n"

    def renderExamples(xs: List[Example]): String =
      xs.map(e => "* " + e.text.trim).mkString("|").attr(Foreground.color(107))

    def renderTranslations(xs: List[String]): String =
      xs.mkString("|").attr(Foreground.color(103))

    def renderCardValue(d: Data): String = d.value.attr(Format.Bold | Foreground.Cyan) +
      d.transcript.map(t => ("[" + t + "]").foreground(24)).getOrElse("") + " " +
      d.kind.getOrElse("").attr(Foreground.Yellow)

    def renderNoneCard: String = "<none>".attr(Foreground.Yellow)
  }


  import com.zx80live.mod.fastcards.ExamController.Renderers._


  def main(args: Array[String]): Unit = parseArgs(args).map { config =>
    println("\n" + config.files.map(_.getName).mkString(", ").attr(Foreground.color(237)))

    readCards(config).map { cards =>
      val badMode = config.files.map(getFileExtension).collect { case Some("bad") | Some("mid") => true}.length > 0
      val badFilePrefixOpt: Option[String] = if (!badMode) Some(config.files.map(_.getName).mkString("_")) else None

      println(renderStartExam(badFilePrefixOpt))

      val state: Deck = exam(cards)
      //todo write state
    }
  }


  def exam(cards: List[Card]): Deck = {
    val con = new R()
    var state = Deck(cards)

    def printState(d: Deck): Unit = {
      clearLine()

      print("\r" + d.current.map {
        case c: InfoSide => renderExamples(c.data.examples)
        case c: BackSide => renderTranslations(c.data.translations)
        case c: Card => renderCardValue(c.data)
      }.getOrElse(renderNoneCard))
    }


    while (!state.isInstanceOf[EmptyStock]) {
      printState(state)

      state = transition(Event(con.readVirtualKey(), state))
    }
    state
  }


  def clearLine(): Unit = print("\r" + (for (i <- 0 until 155) yield " ").mkString(""))

}
