package com.zx80live.mod.fastcards

object ExamController extends ConsoleReader with ExamFSM {


  import Events._

  def main(args: Array[String]): Unit = {
    val c0 = Card(Data(value = "v0", translations = List("t0")))
    val c1 = Card(Data(value = "v1", translations = List("t1")))
    val c2 = Card(Data(value = "v2", translations = List("t2")))
    val c3 = Card(Data(value = "v3", translations = List("t3")))
    val c4 = Card(Data(value = "v4", translations = List("t4")))


    start(List(c0, c1, c2, c3, c4))
  }

  def start(cards: List[Card]): Unit = {

    implicit val passLimit: Int = 2
    var state = State(cards)

    handleKeys { code =>

      print("\r" + state.current.map(_.data.value) + "                      ")

      code match {
        case Code.RIGHT =>
          state = state.next
          state
        case Code.LEFT =>
          state = state.prev
          state
        case Code.SPACE =>
          state = state.estimateFalse
          state
        case Code.ENTER =>
          state = state.estimateTrue(0L)
          state
        case Code.I =>
          state
        case Code.S =>
          state
        case Code.D =>
          state = state.drop
          state

//        case Code.CTRL_D =>
//          BreakEvent
        //case code@_ => println(code)
      }

    }
  }
}
