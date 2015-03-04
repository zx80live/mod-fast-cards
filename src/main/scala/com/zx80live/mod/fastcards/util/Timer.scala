package com.zx80live.mod.fastcards.util

object Timer {
  var time: Option[Long] = None

  def start: Unit =
    time = Some(System.currentTimeMillis())

  def stop: Long =
    time.map(t => System.currentTimeMillis() - t).getOrElse(0)

}
