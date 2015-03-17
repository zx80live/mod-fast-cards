package com.zx80live.mod.fastcards

import scala.tools.jline.console.{ConsoleReader => R}
import scala.util.{Failure, Success}

object ExamController extends ExamExtensions with ExamFSM with ArgumentParser {

  import com.zx80.mod.util.console.ConsoleCSS._


  def main(args: Array[String]): Unit = parseArgs(args).map { config =>
    println("\n" + config.files.map(_.getName).mkString(", ").attr(Foreground.color(237)))

    readCards(config).map(exam) match {
      case Success(result: Deck) =>
      //todo write bad results
      //    val badMode = config.files.map(getFileExtension).collect { case Some("bad") | Some("mid") => true}.length > 0
      //    val badFilePrefix: Option[String] = if (!badMode) Some(config.files.map(_.getName).mkString("_")) else None
      case Failure(e) =>
        println(e)
    }
  }


  def exam(cards: List[Card]): Deck = {
    val con = new R()
    var state = Deck(cards)

    def printState(d: Deck): Unit = {
      print("\r" + d.current.map {
        case c: InfoSide => c.data.examples.map(e => "* " + e.text).mkString("|")
        case c: BackSide => c.data.translations.mkString("|")
        case c: Card => c.data.value
      }.getOrElse("<none>") + "                      ")
    }


    while (!state.isInstanceOf[EmptyStock]) {
      printState(state)

      state = transition(Event(con.readVirtualKey(), state))
    }
    state
  }


}
