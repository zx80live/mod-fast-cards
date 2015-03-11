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

  case class Event(code: Int, state: State)

  def fsm(initState: State)(f: Event => State): Unit = {
    val con = new R()
    var state = initState

    while (!state.isInstanceOf[EmptyStock]) {
      state = f(Event(con.readVirtualKey(), state))
    }
  }

  def start(cards: List[Card]): Unit = {

    implicit val passLimit: Int = 2


    fsm(State(cards)) { evt =>

      print("\r" + evt.state.current.map(_.data.value) + "                      ")

      evt.code match {
        case Code.RIGHT =>
          evt.state.next
        case Code.LEFT =>
          evt.state.prev
        case Code.SPACE =>
          evt.state.estimateFalse
        case Code.ENTER =>
          evt.state.estimateTrue(0L)
        case Code.I =>
          evt.state
        case Code.S =>
          evt.state
        case Code.D =>
          evt.state
        case Code.CTRL_D =>
          evt.state.dropAll
      }
    }
  }
}
