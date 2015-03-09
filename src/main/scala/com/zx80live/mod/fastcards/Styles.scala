package com.zx80live.mod.fastcards

trait Styles {

  import com.zx80.mod.util.console.ConsoleCSS._

  val cssWord = Foreground.Cyan
  val cssWordType = Foreground.Yellow
  val cssStatusBar = Foreground.DarkGray

  val cssBest = Foreground.color(246)
  val cssMid = Foreground.color(246)
  val cssBad = Foreground.color(246)

  val cssHead = Foreground.color(236) | Format.Underlined | Format.Bold
  val cssBestHead = cssHead
  val cssMidHead = cssHead
  val cssBadHead = cssHead

  //val cssCtxBg = Background.color(234)
  //val cssCtxBg = Background.color(0)
  val cssCtxBg = Background.color(233)
  val ccsCtxFg = Foreground.color(236)
  val cssQuestion = Foreground.color(82) | Format.Bold

  def btn(name: String, text: String) =
    name.attr(ccsCtxFg | cssCtxBg | Format.Bold) + s"-$text".attr(ccsCtxFg | cssCtxBg)

  val delim = "|".attr(Foreground.color(234) | cssCtxBg)

}
