package com.zx80live.mod.fastcards

import scala.language.implicitConversions
import scala.tools.jline.console.{ConsoleReader => R}

trait ConsoleReader extends ExamFSM {

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

  object Events {

    trait ConsoleEvent

    sealed trait BreakEvent extends ConsoleEvent

    sealed trait ContinueEvent extends ConsoleEvent

    sealed trait NextEvent extends ContinueEvent

    sealed trait PrevEvent extends ContinueEvent

    sealed trait TrueCardEvent extends ContinueEvent

    sealed trait FalseCardEvent extends ContinueEvent

    sealed trait StatisticEvent extends ContinueEvent

    sealed trait DropEvent extends ContinueEvent

    object BreakEvent extends BreakEvent

    object ContinueEvent extends ContinueEvent

    object NextEvent extends ContinueEvent

    object PrevEvent extends ContinueEvent

    object StatisticEvent extends ContinueEvent

    object DropEvent extends ContinueEvent

    object TrueCardEvent extends ContinueEvent

    object FalseCardEvent extends ContinueEvent

  }

  import Events._

  implicit def unit2Event(u: Unit): ConsoleEvent = ContinueEvent

  def handleKeys(f: Int => State): Unit = {
    val con = new R()
    while (f(con.readVirtualKey()) match {
      case e: EmptyStock => false
      case e: State => true
    }) {}
  }
}
