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

  def main(args: Array[String]): Unit =

    args.toList match {
      case file :: tail =>
        val cards = CardsReader.read(file)

        tail match {
          case kind :: Nil => exam(cards.filter(_.kind == Some(kind)))
          case _ => exam(cards)
        }

      case _ => println("[ERROR] enter cards file")
    }


  def exam(cards: List[Card]): Unit = {

    println("\nstart exam".attr(Format.Bold | Foreground.color(70)) + "\n")

    var stock: List[Card] = Random.shuffle(cards)
    var discard: List[Card] = Nil
    val con: ConsoleReader = new ConsoleReader()
    var run = true
    val limit = 1

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
      val top = 2000
      val low = 7000

      def average(list: List[Long]): Double = if (list.nonEmpty) {
        list.sum / list.length
      } else Long.MaxValue

      def grow(xs: List[String], max: Int): List[String] = {
        var gs: List[String] = Nil
        for (i <- 0 until scala.math.abs(xs.length - max)) {
          gs = "_" :: gs
        }
        xs ::: gs
      }

      var topWords = xs.filter(e => average(e.times) <= top).map(_.value).sorted
      var midWords = xs.filter(e => average(e.times) > top && average(e.times) < low).map(_.value).sorted
      var lowWords = xs.filter(e => average(e.times) >= low).map(_.value).sorted


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
      printHelp()
    }

    var viewer: Viewer = valueViewer

    printHelp()

    while (run) {
      clearLine()
      print(s"\r\u001b[90m[${pointer + 1}/${stock.length + 1}] -${current(stock).statistic}- " + Console.RESET + viewer.view(current(stock)))


      con.readVirtualKey() match {
        //case 2 if viewer.isInstanceOf[ValueViewer] => stock = prev(stock)
        //case 6 if viewer.isInstanceOf[ValueViewer] => stock = next(stock)
        case 32 => viewer match {
          //FALSE, next
          case v: ExampleViewer => viewer = valueViewer
          case v: ValueViewer => viewer = transViewer
          case v: TransViewer =>
            current(stock).statistic = 0
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

              if (card.statistic < limit) {
                card.statistic = card.statistic + 1
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
            case v: ValueViewer => viewer = exampleViewer
            case _ => viewer = valueViewer
          }

        case 119 =>
          viewer = valueViewer

        case 115 => printStatistic()

        case scala.tools.jline.console.Key.CTRL_D.code =>
          printStatistic()
          run = false
        case k@_ => //println("\n" + k)
      }
    }
  }


  def printHelp(): Unit = {
    //println( s"""\n\n\n   ${"CTRL+D".foreground(127)}: \t ${"exit".attr(cssStatusBar)}""")
    //println( s"""      ←/→${": \t next/prev card".attr(cssStatusBar)}.""")
    //println( s"""        i${": \t card info".attr(cssStatusBar)} """)
    //println( s"""    ${"Enter".attr(Foreground.Red)}${": \t true/remove card".attr(cssStatusBar)}""")
    //println( s"""    ${"Space".attr(Foreground.Green)}${": \t flip card -> false/skip card\n".attr(cssStatusBar)}""")

    val bg = Background.color(234)

    def btn(name: String, text: String) =
      name.attr(Foreground.DarkGray | bg | Format.Bold) + s" - $text ".attr(Foreground.DarkGray | bg)

    val delim = " | ".attr(Foreground.color(237) | bg)

    println(
      btn(" Enter", "flip/true") + delim +
        btn("Space", "flip/false") + delim +
        btn("i", "card examples") + delim +
        btn("s", "statistic") + delim +

        btn("CTRL+D", "exit"))
    println()
  }


  def clearLine(): Unit = {
    print("\r")
    for (i <- 0 until 155) print(" ")
  }

  trait Viewer {
    def view(c: Card): String
  }

  class ValueViewer extends Viewer {
    def view(c: Card): String = Console.BOLD + Console.CYAN + c.value + Console.RESET + Console.YELLOW + " " + c.kind.getOrElse("") + Console.RESET + " "
  }

  class TransViewer extends Viewer {
    def view(c: Card): String = c.translations.mkString(" | ").foreground(103)
  }

  class ExampleViewer extends Viewer {
    def view(c: Card): String = "\u001b[38;5;107m" + c.examples.map(e => "*" + e.text.trim).mkString(" ") + Console.RESET
  }

  val valueViewer = new ValueViewer
  val transViewer = new TransViewer
  val exampleViewer = new ExampleViewer

}
