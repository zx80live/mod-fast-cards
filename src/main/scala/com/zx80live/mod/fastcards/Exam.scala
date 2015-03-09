package com.zx80live.mod.fastcards

import com.zx80live.mod.fastcards.util.Timer

import scala.tools.jline.console.ConsoleReader
import scala.util.Random

object Exam {

  import com.zx80.mod.util.console.ConsoleCSS._

  val defaultExamFile = "words.txt"
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

  val passCount = 1
  val topLimitMs = 3000
  val lowLimitMs = 8000
  val badTime = 1000 * 60 * 60

  var ruEn = true


  def average(list: List[Long]): Double = if (list.nonEmpty) {
    list.sum / list.length
  } else badTime

  implicit class CardListExtension(val xs: List[Card]) {
    def filterTop = xs.filter(e => average(e.times) <= topLimitMs)

    def filterMid = xs.filter(e => average(e.times) > topLimitMs && average(e.times) < lowLimitMs)

    def filterLow = xs.filter(e => average(e.times) >= lowLimitMs)
  }

  import java.io.File

  case class Config(files: Seq[File] = Seq(), enRu: Boolean = false, filter: Seq[String] = Seq())

  val parser = new scopt.OptionParser[Config]("scopt") {
    head("Fast-cards", "3.x")
    arg[Seq[File]]("<file>...") unbounded() action { (x, c) =>
      c.copy(files = c.files.++:(x))
    } text "optional unbounded args"
    opt[Unit]("en-ru") action { (_, c) =>
      c.copy(enRu = true)
    } text "en-ru mode"
    opt[Seq[String]]('f', "filter") valueName "<jar1>,<jar2>..." action { (x, c) =>
      c.copy(filter = x)
    } text "jars to include"
  }

  def getFileExtension(file: File): Option[String] = file.getName.split( """\.""").lastOption


  def main(args: Array[String]): Unit = {

    parser.parse(args, Config()) match {
      case Some(config) =>

        println("\n" + config.files.map(_.getName).mkString(", ").attr(Foreground.color(237)))


        val badMode = config.files.map(getFileExtension).collect { case Some("bad") | Some("mid") => true}.length > 0
        val badFilePrefix =
          if (!badMode)
            Some(config.files.map(_.getName).mkString("_"))
          else None


        val cards: List[Card] = config.files.map(CardsReader.read).flatten.toList

        val filtered = if (config.filter.nonEmpty) {
          val filter: Seq[Some[String]] = config.filter.map(Some(_))
          cards.filter(c => filter.contains(c.kind))
        } else cards

        println("badFilePrefix", badFilePrefix)
        exam(filtered, badFilePrefix)

      case None =>
        println("[ERROR] enter cards file")
    }
  }

  def exam(cards: List[Card], badFilePrefixOpt: Option[String]): Unit = {
    Timer.start
    println("\nstart exam".attr(Format.Bold | Foreground.color(70)) + badFilePrefixOpt.map(_ => "").getOrElse(" " + "repeat bad".attr(Foreground.color(22))) + "\n")

    var stock: List[Card] = Random.shuffle(cards)
    var discard: List[Card] = Nil
    val con: ConsoleReader = new ConsoleReader()
    var run = true


    var pointer = 0

    def current(stock: List[Card]): Card = stock.head

    def next(stock: List[Card]): List[Card] = {
      Timer.start
      if (pointer == stock.length - 1)
        pointer = 0
      else
        pointer = pointer + 1
      stock.tail :+ stock.head
    }

    //    def prev(stock: List[Card]): List[Card] = {
    //      if (pointer == 0)
    //        pointer = stock.length - 1
    //      else
    //        pointer = pointer - 1
    //      stock.last :: stock.take(stock.length - 1)
    //    }

    def remove(stock: List[Card]): List[Card] = stock.tail

    def pointerView = {
      stock.zipWithIndex.map { e =>
        if (e._2 == pointer) Console.BOLD + "\u001b[90m|" + Console.RESET else "\u001b[38;5;235m|" + Console.RESET
      }.mkString("")
    }

    def printStatistic(): Unit = {
      val xs = discard ::: stock






      def grow(xs: List[String], max: Int): List[String] = {
        var gs: List[String] = Nil
        for (i <- 0 until scala.math.abs(xs.length - max)) {
          gs = "-" :: gs
        }
        xs ::: gs
      }


      var topWords = xs.filterTop.map(_.value).sorted
      var midWords = xs.filterMid.map(_.value).sorted
      var lowWords = xs.filterLow.map(_.value).sorted


      val maxSize = List(topWords.length, midWords.length, lowWords.length).max
      topWords = grow(topWords, maxSize)
      midWords = grow(midWords, maxSize)
      lowWords = grow(lowWords, maxSize)

      val result: List[((String, String), String)] = topWords.zip(midWords).zip(lowWords)


      clearLine()
      val w = 40
      val h = "        "
      println("\n\n")
      printf(s"\n$h%${w}s $h%${w}s $h%${w}s", "good".attr(cssBestHead), "middle".attr(cssMidHead), "bad".attr(cssBadHead))
      result foreach { case ((t, m), l) =>
        printf(s"\n%${w}s %${w}s %${w}s", t.attr(cssBest), m.attr(cssMid), l.attr(cssBad))
      }

      println("\n")
      //printHelp()
    }

    var viewer: Viewer = valueViewer

    //printHelp()

    val cssProgress = Foreground.color(236) | cssCtxBg
    var previousViewer: Option[Viewer] = None
    while (run) {
      clearLine()
      print("\r" + s"${pointer + 1}/${stock.length + 1} ".attr(cssProgress) + viewer.view(current(stock)))


      con.readVirtualKey() match {
        //case 2 if viewer.isInstanceOf[ValueViewer] => stock = prev(stock)
        //case 6 if viewer.isInstanceOf[ValueViewer] => stock = next(stock)
        case 32 => viewer match {
          //FALSE, next
          case v: ExampleViewer => viewer = valueViewer
          case v: ValueViewer => viewer = transViewer
          case v: TransViewer =>
            current(stock).passCount = 0
            current(stock).times = badTime :: current(stock).times
            stock = next(stock)
            viewer = valueViewer
        }
        case 10 => // TRUE, remove card

          viewer match {
            case v: ExampleViewer => viewer = valueViewer
            case v: ValueViewer => viewer = transViewer
            case v: TransViewer =>
              val card = current(stock)
              card.times = Timer.stop :: card.times

              if (card.passCount < passCount) {
                card.passCount = card.passCount + 1
                stock = next(stock)
              } else if (stock.length > 1) {
                stock = remove(stock)
                discard = card :: discard
              } else {
                printStatistic()
                run = false
              }
              viewer = valueViewer
          }

        case 105 =>

          viewer match {
            case v: ExampleViewer => viewer = previousViewer.getOrElse(valueViewer)
            case _ =>
              previousViewer = Some(viewer)
              viewer = exampleViewer
          }

        case 119 =>
          viewer = valueViewer

        case 115 => printStatistic()

        case 100 => stock = remove(stock)

        case 109 => stock.filterMid

        case 98 => stock = stock.filterLow

        case scala.tools.jline.console.Key.CTRL_D.code =>
          printStatistic()
          run = false
        case k@_ => println("\n" + k)
      }
    }


    badFilePrefixOpt.foreach { name =>
      val xs = discard ::: stock
      val xsMid = xs.filterMid
      val xsBad = xs.filterLow

      if (xsMid.nonEmpty)
        CardsWriter.write(xsMid, new File(name + ".mid"))

      if (xsBad.nonEmpty)
        CardsWriter.write(xsBad, new File(name + ".bad"))
    }
  }


  def printHelp(): Unit = {
    println(
      btn(" Enter", "flip/true") + delim +
        btn("Space", "flip/false") + delim +
        btn("i", "card examples") + delim +
        btn("d", "drop card") + delim +
        btn("s", "statistic") + delim +

        btn("CTRL+D", "exit"))
    println()
  }


  //val cssCtxBg = Background.color(234)
  //val cssCtxBg = Background.color(0)
  val cssCtxBg = Background.color(233)
  val ccsCtxFg = Foreground.color(236)
  val cssQuestion = Foreground.color(82) | Format.Bold

  def btn(name: String, text: String) =
    name.attr(ccsCtxFg | cssCtxBg | Format.Bold) + s"-$text".attr(ccsCtxFg | cssCtxBg)

  val delim = "|".attr(Foreground.color(234) | cssCtxBg)

  def clearLine(): Unit = {
    print("\r")
    for (i <- 0 until 155) print(" ")
  }

  trait Viewer {
    def view(c: Card): String
  }

  def sideValue(c: Card, mode: Boolean) = {
    if (mode)
      c.translations.mkString(" | ").foreground(103)
    else {
      c.value.attr(Format.Bold | Foreground.Cyan) +
        c.transcript.map(t => ("[" + t + "]").foreground(24)).getOrElse("") + " " +
        c.kind.getOrElse("").attr(Foreground.Yellow)
    }
  }

  class ValueViewer extends Viewer {
    def view(c: Card): String = {
      btn("enter", "flip") + delim + btn("space", "flip ") + "" + " ▹".attr(Foreground.color(22) | cssCtxBg) + " " + sideValue(c, ruEn)
      //      c.value.attr(Format.Bold | Foreground.Cyan) +
      //        c.transcript.map(t => ("[" + t + "]").foreground(24)).getOrElse("") + " " +
      //        c.kind.getOrElse("").attr(Foreground.Yellow)
    }
  }

  class TransViewer extends Viewer {
    def view(c: Card): String =
      btn("enter", " ok ") + delim + btn("space", "false") + "" + " ▸".attr(Foreground.color(22) | cssCtxBg) + " " + sideValue(c, !ruEn)

    //c.translations.mkString(" | ").foreground(103)
  }

  class ExampleViewer extends Viewer {
    def view(c: Card): String = {
      (if (c.examples.nonEmpty)
        c.examples.map(e => "*" + e.text.trim).mkString(" ")
      else
        "<has not examples>").foreground(107)
    }
  }

  val valueViewer = new ValueViewer
  val transViewer = new TransViewer
  val exampleViewer = new ExampleViewer

}
