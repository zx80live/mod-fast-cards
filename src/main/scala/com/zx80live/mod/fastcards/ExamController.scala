package com.zx80live.mod.fastcards

import scala.tools.jline.console.{ConsoleReader => R}

object ExamController extends ExamExtensions with ExamFSM {



  def main(args: Array[String]): Unit = {
    val c0 = Card(Data(value = "v0", translations = List("t0")))
    val c1 = Card(Data(value = "v1", translations = List("t1")))
    val c2 = Card(Data(value = "v2", translations = List("t2")))
    val c3 = Card(Data(value = "v3", translations = List("t3")))
    val c4 = Card(Data(value = "v4", translations = List("t4")))


    start(List(c0, c1, c2, c3, c4))
  }


  def start(cards: List[Card]): Unit = {
    val con = new R()
    var state = Deck(cards)

    while (!state.isInstanceOf[EmptyStock]) {
      printState(state)

      state = transition(Event(con.readVirtualKey(), state))
    }
  }

  private def printState(d: Deck): Unit = {
    print("\r" + d.current.map {
      case c: InfoSide => c.data.examples.map(e => "* " + e.text).mkString("|")
      case c: BackSide => c.data.translations.mkString("|")
      case c: Card => c.data.value
    }.getOrElse("<none>") + "                      ")
  }


}
