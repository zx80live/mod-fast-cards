package com.zx80live.mod.fastcards

import com.zx80.mod.util.console.ConsoleCSS.Foreground
import com.zx80live.mod.fastcards.util.CardsReader

import scala.tools.jline.console.{ConsoleReader => R}
import scala.util.{Failure, Success, Try}

object ExamController extends ExamExtensions with ExamFSM with ArgumentParser {

  import com.zx80.mod.util.console.ConsoleCSS._


  def main(args: Array[String]): Unit = parseArgs(args).map { config =>
    println("\n" + config.files.map(_.getName).mkString(", ").attr(Foreground.color(237)))

    //    val badMode = config.files.map(getFileExtension).collect { case Some("bad") | Some("mid") => true}.length > 0
    //    val badFilePrefix: Option[String] = if (!badMode) Some(config.files.map(_.getName).mkString("_")) else None
    //

    readCards(config) match {
      case Success(cards) => exam(cards)
      case Failure(f) => println(f)
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
