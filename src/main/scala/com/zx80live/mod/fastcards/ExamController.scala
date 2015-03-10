package com.zx80live.mod.fastcards

trait ExamController extends ConsoleReader {

  import Events._

  def start: Unit = {
    handleKeys {
      case Code.SPACE =>
        println("SpaceEvent")
      case Code.ENTER =>
        println("EnterEvent")
      case Code.CTRL_D =>
        println("ExitEvent")
        BreakEvent
      case code@_ =>
        println(code)
    }
  }
}
