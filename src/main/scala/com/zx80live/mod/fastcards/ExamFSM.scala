package com.zx80live.mod.fastcards

trait ExamFSM {
  this: ExamExtensions =>

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

  case class Event(code: Int, state: Deck)

  implicit val passLimit: Int = 2

  def transition(evt: Event): Deck = evt match {
    case Event(Code.RIGHT, s) => s.resetCurrent.next
    case Event(Code.LEFT, s) => s.resetCurrent.prev

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
    case Event(Code.I, s) =>
      s.current match {
        case Some(c: InfoSide) => s.reverseCurrent
        case _ => s.infoCurrent
      }

    case Event(Code.S, s) => s
    case Event(Code.D, s) => s
    case Event(Code.CTRL_D, s) => s.dropAll
    case Event(_, s) => s
  }
}
