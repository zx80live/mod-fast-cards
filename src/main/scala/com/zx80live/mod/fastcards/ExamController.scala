package com.zx80live.mod.fastcards

import java.io.File

import scala.tools.jline.console.{ConsoleReader => R}

object ExamController extends ExamExtensions with ArgumentParser {

  import com.zx80.mod.util.console.ConsoleCSS._
  import com.zx80live.mod.fastcards.ExamController.Renderer._


  def main(args: Array[String]): Unit = parseArgs(args).map { config =>
    renderFiles(config.files)

    readCards(config).map { cards =>
      val badMode = config.files.map(getFileExtension).collect { case Some("bad") | Some("mid") => true }.length > 0
      val badFilePrefixOpt: Option[String] = if (!badMode) Some(config.files.map(_.getName).mkString("_")) else None

      renderStartExam(badFilePrefixOpt)

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

    implicit val passLimit: Int = 2

    while (!state.isInstanceOf[EmptyStock]) {
      printState(state)

      state = con.readVirtualKey() match {
        case Code.RIGHT =>
          state.resetCurrent.next
        case Code.LEFT =>
          state.resetCurrent.prev
        case Code.SPACE =>
          state.current match {
            case Some(c: BackSide) =>
              state.estimateFalse
            case _ => state.backCurrent
          }
        case Code.ENTER =>
          state.current match {
            case Some(c: BackSide) =>
              state.estimateTrue(0L)

            case _ => state.backCurrent
          }
        case Code.I =>
          state.current match {
            case Some(c: InfoSide) => state.reverseCurrent
            case _ => state.infoCurrent
          }
        case Code.S => state
        case Code.D => state.drop
        case Code.CTRL_D => state.dropAll
        case _ => state
      }
    }
    state
  }

  object Code {
    val LEFT = 2
    val RIGHT = 6
    val SPACE = 32
    val ENTER = 10
    val I = 105
    val S = 115
    val D = 100
    val CTRL_D = scala.tools.jline.console.Key.CTRL_D.code
  }

  object Renderer {
    val consoleWidth = 155

    def renderFiles(xs: Seq[File]): Unit =
      println("\n" + xs.map(_.getName).mkString(", ").attr(Foreground.color(237)))

    def renderStartExam(badFilePrefixOpt: Option[String]): Unit =
      println("\nstart exam".attr(Format.Bold | Foreground.color(70)) + badFilePrefixOpt.map(_ => "").getOrElse(" " + "repeat bad".attr(Foreground.color(22))) + "\n")

    def renderExamples(xs: List[Example]): String =
      xs.map(e => "* " + e.text.trim).mkString("|").attr(Foreground.color(107))

    def renderTranslations(xs: List[String]): String =
      xs.mkString("|").attr(Foreground.color(103))

    def renderCardValue(d: Data): String = d.value.attr(Format.Bold | Foreground.Cyan) +
      d.transcript.map(t => ("[" + t + "]").foreground(24)).getOrElse("") + " " +
      d.kind.getOrElse("").attr(Foreground.Yellow)

    def renderNoneCard: String = "<none>".attr(Foreground.Yellow)

    def clearLine(): Unit = print("\r" + (for (i <- 0 until consoleWidth) yield " ").mkString(""))
  }


}
