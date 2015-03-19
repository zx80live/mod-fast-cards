package com.zx80live.mod.fastcards

import java.io.File

import com.zx80live.mod.fastcards.util.CardsWriter

import scala.tools.jline.console.{ConsoleReader => R}

object ExamController extends ExamExtensions with ArgumentParser {

  import com.zx80.mod.util.console.ConsoleCSS._
  import com.zx80live.mod.fastcards.ExamController.Renderer._
  import com.zx80live.mod.fastcards.util.CollectionUtils._


  def main(args: Array[String]): Unit = parseArgs(args).map { config =>
    renderFiles(config.files)

    readCards(config).map { cards =>
      val badMode = config.files.map(getFileExtension).collect { case Some("bad") | Some("mid") => true }.length > 0
      val badFilePrefixOpt: Option[String] = if (!badMode) Some(config.files.map(_.getName).mkString("_")) else None

      renderStartExam(badFilePrefixOpt)

      val state: Deck = exam(cards)
      printStatistic(state.statistic)


      badFilePrefixOpt.foreach { name =>

        val xsMid = state.statistic.middle
        val xsBad = state.statistic.bad

        if (xsMid.nonEmpty)
          CardsWriter.write(xsMid, new File(name + ".mid"))

        if (xsBad.nonEmpty)
          CardsWriter.write(xsBad, new File(name + ".bad"))
      }
    }
  }


  def exam(cards: List[Card]): Deck = {
    val con = new R()
    var state = Deck(cards)

    import Actions.{Event, transition, Timer}

    Timer.start()
    while (!state.isInstanceOf[EmptyStock]) {
      printState(state)

      state = transition(Event(con.readVirtualKey(), state))
    }
    state
  }

  object Actions {

    object Timer {
      var t0: Option[Long] = None

      def start(): Unit = t0 = Some(System.currentTimeMillis())

      def getTime: Long = t0.map(t => System.currentTimeMillis() - t).getOrElse(0L)
    }

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

    val caseRight: PartialFunction[Event, Deck] = {
      case Event(Code.RIGHT, s) =>
        Timer.start()
        s.resetCurrent.next
    }

    val caseLeft: PartialFunction[Event, Deck] = {
      case Event(Code.LEFT, s) =>
        Timer.start()
        s.resetCurrent.prev
    }

    val caseSpace: PartialFunction[Event, Deck] = {
      case Event(Code.SPACE, s) =>
        s.current match {
          case Some(c: BackSide) =>
            Timer.start()
            s.estimateFalse
          case _ => s.backCurrent
        }
    }

    val caseEnter: PartialFunction[Event, Deck] = {
      case Event(Code.ENTER, s) =>
        s.current match {
          case Some(c: BackSide) =>
            val time = Timer.getTime
            Timer.start()
            s.estimateTrue(time)

          case _ => s.backCurrent
        }
    }

    val caseInfo: PartialFunction[Event, Deck] = {
      case Event(Code.I, s) =>
        s.current match {
          case Some(c: InfoSide) => s.reverseCurrent
          case _ => s.infoCurrent
        }
    }

    val caseStatistic: PartialFunction[Event, Deck] = {
      case Event(Code.S, s) =>
        printStatistic(s.statistic)
        s
    }

    val caseDrop: PartialFunction[Event, Deck] = {
      case Event(Code.D, s) =>
        Timer.start()
        s.drop
    }

    val caseDropAll: PartialFunction[Event, Deck] = {
      case Event(Code.CTRL_D, s) =>
        Timer.start()
        s.dropAll
    }

    val wildcard: PartialFunction[Event, Deck] = {
      case Event(_, s) => s
    }

    def transition = caseRight orElse caseLeft orElse
      caseSpace orElse caseEnter orElse
      caseInfo orElse caseStatistic orElse caseDrop orElse caseDropAll orElse wildcard
  }


  object Renderer {
    val consoleWidth = 155

    def renderFiles(xs: Seq[File]): Unit =
      println("\n" + xs.map(_.getName).mkString(", ").attr(Foreground.color(237)))

    def renderStartExam(badFilePrefixOpt: Option[String]): Unit =
      println("\nstart exam".attr(Format.Bold | Foreground.color(70)) + badFilePrefixOpt.map(_ => "").getOrElse(" " + "repeat bad".attr(Foreground.color(22))) + "\n")

    def renderExamples(xs: List[Example]): String =
      xs.map(e => "* " + e.text.trim).mkString("|").attr(Foreground.color(107))

    def renderTranslations(xs: List[String]): String =
      xs.mkString("|").attr(Foreground.color(103))

    def renderCardValue(d: Data): String = d.value.attr(Format.Bold | Foreground.Cyan) +
      d.transcript.map(t => ("[" + t + "]").foreground(24)).getOrElse("") + " " +
      d.kind.getOrElse("").attr(Foreground.Yellow)

    def renderNoneCard: String = "<none>".attr(Foreground.Yellow)

    def clearLine(): Unit = print("\r" + (for (i <- 0 until consoleWidth) yield " ").mkString(""))

    def printState(d: Deck): Unit = {
      clearLine()

      print("\r" + d.current.map {

        case c: InfoSide =>
          progress(d) + renderExamples(c.data.examples)

        case c: BackSide =>
          progress(d) + btn("enter", " ok ") + delim + btn("space", "false") + "" + " ▸".attr(Foreground.color(22) | cssCtxBg) + " " +
            renderTranslations(c.data.translations)

        case c: Card =>
          progress(d) + btn("enter", "flip") + delim + btn("space", "flip ") + "" + " ▹".attr(Foreground.color(22) | cssCtxBg) + " " +
            renderCardValue(c.data)

      }.getOrElse(renderNoneCard))
    }

    def printStatistic(s: Statistic): Unit = {
      val bestWords = s.best.map(_.data.value).sorted
      val midWords = s.middle.map(_.data.value).sorted
      val badWords = s.bad.map(_.data.value).sorted

      val maxSize = List(bestWords.length, midWords.length, badWords.length).max
      val result: List[((String, String), String)] =
        bestWords.fill("-", maxSize)
          .zip(midWords.fill("-", maxSize))
          .zip(badWords.fill("-", maxSize))

      clearLine()

      val cssHead = Foreground.color(237)


      implicit class LocalStringExt(s: String) {
        def colored: String = s match {
          case "-" => s.attr(Foreground.color(237))
          case _ => s.attr(Foreground.color(247))
        }
      }

      val w = 40
      val h = ""
      println("\n\n")
      printf(s"\n$h%${w}s $h%${w}s $h%${w}s", "good".attr(cssHead), "middle".attr(cssHead), "bad".attr(cssHead))
      result foreach { case ((t, m), l) =>
        printf(s"\n%${w}s %${w}s %${w}s", t.colored, m.colored, l.colored)
      }

      println("\n")

    }

    val cssCtxBg = Background.color(233)
    val ccsCtxFg = Foreground.color(236)

    def btn(name: String, text: String) =
      name.attr(ccsCtxFg | cssCtxBg | Format.Bold) + s"-$text".attr(ccsCtxFg | cssCtxBg)

    val delim = "|".attr(Foreground.color(234) | cssCtxBg)

    def progress(d:Deck):String =
      //progressPercentText(d)
      progressCountText(d)

    def progressPercentText(d: Deck): String = ("[" + (d.discard.length * 100 / d.deck.length) + "%] ").attr(ccsCtxFg | cssCtxBg)

    def progressCountText(d: Deck): String = {
      val percents: Int = d.discard.length * 100 / d.deck.length
      ("[" + d.discard.length + "/" + d.deck.length + "] ").attr(ccsCtxFg | cssCtxBg)
    }

  }


}
