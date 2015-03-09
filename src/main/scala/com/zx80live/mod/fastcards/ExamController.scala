package com.zx80live.mod.fastcards

trait ExamController extends ConsoleReader {

  trait ExamEvent

  trait ExitEvent extends ExamEvent

  val ExitEvent = new ExitEvent {}

  def start: Unit = {
    handleKeys {
      case Code.SPACE =>
        println("SpaceEvent")
      case Code.ENTER =>
        println("EnterEvent")
      case Code.CTRL_D =>
        println("ExitEvent")
      case code@_ =>
        println(code)
    }
  }
}
