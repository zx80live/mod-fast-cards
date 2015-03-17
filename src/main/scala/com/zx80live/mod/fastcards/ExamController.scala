package com.zx80live.mod.fastcards

import scala.tools.jline.console.{ConsoleReader => R}

object ExamController extends ExamFSM {

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

  def main(args: Array[String]): Unit = {
    val c0 = Card(Data(value = "v0", translations = List("t0")))
    val c1 = Card(Data(value = "v1", translations = List("t1")))
    val c2 = Card(Data(value = "v2", translations = List("t2")))
    val c3 = Card(Data(value = "v3", translations = List("t3")))
    val c4 = Card(Data(value = "v4", translations = List("t4")))


    start(List(c0, c1, c2, c3, c4))
  }

  case class Event(code: Int, state: Deck)

  type State = Deck
  type EndState = EmptyStock

  def loop(initState: State)(transition: Event => State): Unit = {
    val con = new R()
    var state = initState

    while (!state.isInstanceOf[EndState]) {
      state = transition(Event(con.readVirtualKey(), state))
    }
  }

  def start(cards: List[Card]): Unit = {

    implicit val passLimit: Int = 2

    loop(Deck(cards)) { evt =>
      printState(evt.state)
      transition(evt)
    }

    def printState(d: Deck): Unit = {
      print("\r" + d.current.map {
        case c: BackSide => c.data.translations.mkString("|")
        case c: Card => c.data.value
      } + "                      ")
    }

    def transition(evt: Event): Deck = evt match {
      case Event(Code.RIGHT, s) => s.next
      case Event(Code.LEFT, s) => s.prev

      case Event(Code.SPACE, s) =>
        s.current match {
          case Some(c: BackSide) => s.estimateFalse
          case _ => s.backCurrent
        }

      case Event(Code.ENTER, s) =>
        s.current match {
          case Some(c: BackSide) => s.estimateTrue(0L)
          case _ => s.backCurrent
        }
      case Event(Code.I, s) => s
      case Event(Code.S, s) => s
      case Event(Code.D, s) => s
      case Event(Code.CTRL_D, s) => s.dropAll
      case Event(_, s) => s
    }


  }
}
