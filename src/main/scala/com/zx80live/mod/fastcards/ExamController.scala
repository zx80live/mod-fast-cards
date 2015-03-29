package com.zx80live.mod.fastcards

import java.io.File

import com.zx80live.mod.fastcards.util.CardsWriter
import com.zx80live.mod.fastcards.util.regex.RegexDecorator

import scala.collection.mutable
import scala.tools.jline.console.{ConsoleReader => R}
import scala.util.{Random, Success, Failure}

object ExamController extends ExamExtensions with ArgumentParser {

  import com.zx80.mod.util.console.ConsoleCSS._
  import com.zx80live.mod.fastcards.ExamController.Renderer._
  import com.zx80live.mod.fastcards.util.CollectionUtils._

  var config: Config = _


  def main(args: Array[String]): Unit = parseArgs(args).map { config =>
    this.config = config

    readCards(config).map { xs => if (!config.noShuffle) Random.shuffle(xs) else xs }.map { cards =>
      renderConfig(config)

      val state: Deck = exam(cards, config.enRu)
      renderEndExam(state)


      config.badFilePrefixOpt.foreach { name =>

        val xsMid = state.statistic.middle
        val xsBad = state.statistic.bad

        if (xsMid.nonEmpty)
          CardsWriter.write(xsMid, new File(name + ".mid"))

        if (xsBad.nonEmpty)
          CardsWriter.write(xsBad, new File(name + ".bad"))
      }
    } match {
      case Success(_) => Unit
      case Failure(e) => println(e.toString.attr(Foreground.Red))
    }
  }


  def exam(cards: List[Card], enRu: Boolean = true): Deck = {
    val con = new R()
    var state = Deck(cards)
    implicit var history: mutable.Stack[Deck] = mutable.Stack[Deck]()


    import Actions.{Event, transition, Timer}

    Timer.start()
    while (!state.isInstanceOf[EmptyStock]) {
      printState(state, enRu)

      val newState: Deck = transition(Event(con.readVirtualKey(), state))
      state = newState
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
      val SPACE = 32
      val ENTER = 10
      val INFO = 105
      val STATISTIC = 115
      val DROP = 100
      val RESET_TIMER = 112
      val SUGGEST = 0
      val UNDO = 8
      val CTRL_D = scala.tools.jline.console.Key.CTRL_D.code
    }

    case class Event(code: Int, state: Deck)

    implicit val passLimit: Int = 2

    def transition(evt: Event)(implicit history: mutable.Stack[Deck]): Deck = evt match {
      case Event(Code.SPACE, s) =>
        s.current match {
          case Some(c: BackSide) =>
            Timer.start()
            history.push(s)
            s.estimateFalse
          case _ => s.backSideCurrent
        }
      case Event(Code.ENTER, s) =>
        s.current match {
          case Some(c: BackSide) =>
            val time = Timer.getTime
            Timer.start()
            history.push(s)
            s.estimateTrue(time)

          case _ => s.backSideCurrent
        }
      case Event(Code.INFO, s) =>
        s.current match {
          case Some(c: InfoSide) => s.reverseSideCurrent
          case _ => s.infoCurrent
        }
      case Event(Code.STATISTIC, s) =>
        printStatistic(s.statistic)
        s
      case Event(Code.DROP, s) =>
        Timer.start()
        s.drop
      case Event(Code.CTRL_D, s) =>
        Timer.start()
        s.dropAll
      case Event(Code.RESET_TIMER, s) => Timer.start(); s
      case Event(Code.UNDO, s) => if (history.nonEmpty) history.pop() else s
      case Event(Code.SUGGEST, s) => s //todo suggest
      case Event(_, s) => s
    }
  }

  object Renderer {

    import RegexDecorator._

    val consoleWidth = 155

    def renderConfig(config: Config): Unit = {
      println("\n" + ((if (config.enRu) "en-ru" else "ru-en") + ": ").attr(Foreground.color(236)) + config.files.map(_.getName).mkString(", ").attr(Foreground.color(237)))
      println("\n" + config.badFilePrefixOpt.map(_ => "start exam").getOrElse("start repeat bad").attr(Format.Bold | Foreground.color(70)) + "\n")
    }

    def renderExamples(xs: List[Example]): String =
      (if (xs.nonEmpty) xs.map(e => "* " + text(e.text.trim)).mkString("|") else "<no-examples>").attr(Foreground.color(107))

    def renderTranslations(xs: List[String]): String =
      text(xs.mkString("|")).attr(Foreground.color(103))

    def renderCardValue(d: Data): String = text(d.value).attr(Format.Bold | Foreground.Cyan) +
      d.transcript.map(t => ("[" + t + "]").foreground(24)).getOrElse("") + " " +
      d.kind.getOrElse("").attr(Foreground.Yellow)

    def renderNoneCard: String = "<none>".attr(Foreground.Yellow)

    def clearLine(): Unit = print("\r" + (for (i <- 0 until consoleWidth) yield " ").mkString(""))

    def printState(d: Deck, enRu: Boolean = true): Unit = {
      clearLine()



      print("\r" + d.current.map {

        case c: InfoSide =>
          progress(d) + renderExamples(c.data.examples)

        case c: BackSide =>
          progress(d) + btn("enter", " ok ") + delim + btn("space", "false") + "" + " ▸".attr(Foreground.color(22) | cssCtxBg | Format.Blink) + " " + (if (enRu) renderTranslations(c.data.translations) else renderCardValue(c.data))


        case c: Card =>
          progress(d) + btn("enter", "flip") + delim + btn("space", "flip ") + "" + " ▹".attr(Foreground.color(22) | cssCtxBg) + " " + (if (enRu) renderCardValue(c.data) else renderTranslations(c.data.translations))


      }.getOrElse(renderNoneCard))
    }

    def renderEndExam(d: Deck): Unit = {
      clearLine()
      println("\nexam success".attr(Format.Bold | Foreground.color(70)) + "\n")
      printStatistic(d.statistic)
    }

    def printStatistic(s: Statistic): Unit = {
      val normalizedLen = 20
      def normalize(s: String): String = if (s.length < normalizedLen) s else s.substring(0, normalizedLen) + ".."

      def value(c: Card): String = normalize(if (config.enRu) c.data.value else c.data.translations.headOption.getOrElse("<none>"))

      val bestWords = s.best.map(value).sorted
      val midWords = s.middle.map(value).sorted
      val badWords = s.bad.map(value).sorted

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

    def progress(d: Deck): String =
    //progressPercentText(d)
      progressCountText(d)

    //progressBar(d)

    def progressPercentText(d: Deck): String = ("[" + (d.discard.length * 100 / d.deck.length) + "%] ").attr(ccsCtxFg | cssCtxBg)

    def progressCountText(d: Deck): String = {
      ("[" + d.discard.length + "/" + d.deck.length + "] ").attr(ccsCtxFg | cssCtxBg)
    }

    def progressBar(d: Deck): String = {
      val percents: Int = d.discard.length * 100 / d.deck.length
      val barLength = 20

      val currentLength = barLength * percents / 100
      val symbol = "▉"
      val space = " ".attr(Background.color(233))
      val bar = ("" :: Nil).fill(symbol, currentLength).fill(space, barLength).mkString("")
      ("" + bar + "").attr(ccsCtxFg | cssCtxBg) + (percents + "% ").attr(ccsCtxFg | cssCtxBg) + ""
    }

    def text(s: String): String = s.decorateAll("`(.*?)`", "[", "]")

  }


}
