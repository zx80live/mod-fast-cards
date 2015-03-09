package com.zx80live.mod.fastcards

import scala.language.implicitConversions
import scala.tools.jline.console.{ConsoleReader => R}

trait ConsoleReader {

  object Code {
    val SPACE = 32
    val ENTER = 10
    val I = 105
    val S = 115
    val D = 100
    val CTRL_D = scala.tools.jline.console.Key.CTRL_D.code
  }

  trait ConsoleEvent

  sealed trait BreakEvent extends ConsoleEvent

  sealed trait ContinueEvent extends ConsoleEvent

  val BreakEvent = new BreakEvent {}
  val ContinueEvent = new ContinueEvent {}

  implicit def unit2Event(u: Unit): ConsoleEvent = ContinueEvent

  def handleKeys(f: Int => ConsoleEvent): Unit = {
    val con = new R()
    while (f(con.readVirtualKey()) match {
      case e: ContinueEvent => true
      case e: BreakEvent => false
    }) {}
  }
}
