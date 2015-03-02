package com.zx80live.mod.fastcards

import scala.tools.jline.console.ConsoleReader
import scala.util.Random

object Exam {

  val defaultExamFile = "words.txt"

  def main(args: Array[String]): Unit = args.toList match {
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
    val limit = 3

    var viewer: Viewer = valueViewer

    printHelp()
    while (run) {
      clearLine()
      print(s"\r\u001b[90m[stock: ${stock.length + 1}] -${current(stock).statistic}- " + Console.RESET + viewer.view(current(stock)))

      con.readVirtualKey() match {
        case 2 => stock = prev(stock)
        case 6 => stock = next(stock)
        case 10 | 102 => viewer match {
          //FALSE, next
          case v: ExampleViewer => viewer = valueViewer
          case v: ValueViewer => viewer = transViewer
          case v: TransViewer =>
            current(stock).statistic = 0
            stock = next(stock)
            viewer = valueViewer
        }
        case 49 | 121 | 32 => // TRUE, remove card
          if (viewer.isInstanceOf[TransViewer]) {

            if (current(stock).statistic < limit) {
              current(stock).statistic = current(stock).statistic + 1
              stock = next(stock)
              viewer = valueViewer
            } else if (stock.length > 1) {
              stock = remove(stock)
            } else {
              run = false
            }
          }

        case 105 =>
          viewer match {
            case v: ValueViewer => viewer = exampleViewer
            case _ => viewer = valueViewer
          }

        case 119 =>
          viewer = valueViewer

        case scala.tools.jline.console.Key.CTRL_D.code =>
          //print("bye")
          run = false
        case k@_ => println("\n\r" + k)
      }
    }
  }

  def current(stock: List[Card]): Card = stock.head

  def next(stock: List[Card]): List[Card] = stock.tail :+ stock.head

  def prev(stock: List[Card]): List[Card] = stock.last :: stock.take(stock.length - 1)

  def remove(stock: List[Card]): List[Card] = stock.tail

  def printHelp(): Unit = {
    println("\n\n\nPress CTRL+D to exit\n")

    println("      ←/→: \t next/prev card")
    println("        i: \t card info")
    println(Console.RED + "    Enter" + Console.RESET + ": \t flip card -> false/skip card")
    println(Console.GREEN + "    Space" + Console.RESET + ": \t true/remove card\n")
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
    def view(c: Card): String = "\u001b[38;5;107m" + c.examples.map(_.text.trim).mkString("* ") + Console.RESET
  }

  val valueViewer = new ValueViewer
  val transViewer = new TransViewer
  val exampleViewer = new ExampleViewer

}
