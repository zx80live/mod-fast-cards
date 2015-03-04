package com.zx80live.mod.fastcards

import scala.tools.jline.console.ConsoleReader
import scala.util.Random

object Exam {

  import com.zx80.mod.util.console.ConsoleCSS._

  val defaultExamFile = "words.txt"
  val cssWord = Foreground.Cyan
  val cssWordType = Foreground.Yellow
  val cssStatusBar = Foreground.DarkGray

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
    var stock: List[Card] = Random.shuffle(cards)
    val con: ConsoleReader = new ConsoleReader()
    var run = true
    val limit = 1

    var pointer = 0

    def current(stock: List[Card]): Card = stock.head

    def next(stock: List[Card]): List[Card] = {
      if (pointer == stock.length - 1)
        pointer = 0
      else
        pointer = pointer + 1
      stock.tail :+ stock.head
    }

    def prev(stock: List[Card]): List[Card] = {
      if (pointer == 0)
        pointer = stock.length - 1
      else
        pointer = pointer - 1
      stock.last :: stock.take(stock.length - 1)
    }

    def remove(stock: List[Card]): List[Card] = stock.tail

    def pointerView = {
      stock.zipWithIndex.map { e =>
        if (e._2 == pointer) Console.BOLD + "\u001b[90m|" + Console.RESET else "\u001b[38;5;235m|" + Console.RESET
      }.mkString("")
    }

    var viewer: Viewer = valueViewer

    printHelp()

    while (run) {
      clearLine()
      print(s"\r\u001b[90m[stock: ${pointer + 1}/${stock.length + 1}] -${current(stock).statistic}- " + Console.RESET + viewer.view(current(stock)))


      con.readVirtualKey() match {
        case 2 if viewer.isInstanceOf[ValueViewer] => stock = prev(stock)
        case 6 if viewer.isInstanceOf[ValueViewer] => stock = next(stock)
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
              if (current(stock).statistic < limit) {
                current(stock).statistic = current(stock).statistic + 1
                stock = next(stock)
              } else if (stock.length > 1) {
                stock = remove(stock)
              } else {
                printStatistic
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

        case 115 => printStatistic

        case scala.tools.jline.console.Key.CTRL_D.code =>
          printStatistic
          run = false
        case k@_ => //println("\n" + k)
      }
    }
  }


  def printHelp(): Unit = {
    println( s"""\n\n\n   ${"CTRL+D".foreground(127)}: \t ${"exit".attr(cssStatusBar)}""")
    println( s"""      ←/→${": \t next/prev card".attr(cssStatusBar)}.""")
    println( s"""        i${": \t card info".attr(cssStatusBar)} """)
    println( s"""    ${"Enter".attr(Foreground.Red)}${": \t true/remove card".attr(cssStatusBar)}""")
    println( s"""    ${"Space".attr(Foreground.Green)}${": \t flip card -> false/skip card\n".attr(cssStatusBar)}""")
  }

  def printStatistic():Unit = {
    println("print statistic")
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
    def view(c: Card): String = c.translations.mkString(" | ")
  }

  class ExampleViewer extends Viewer {
    def view(c: Card): String = "\u001b[38;5;107m" + c.examples.map(e => "*" + e.text.trim).mkString(" ") + Console.RESET
  }

  val valueViewer = new ValueViewer
  val transViewer = new TransViewer
  val exampleViewer = new ExampleViewer

}
