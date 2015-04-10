package com.zx80live.mod.fastcards

import scala.language.implicitConversions
import scala.util.Random

//todo implicit event listener
trait Exam {

  type Time = Long

  case class Card(data: CardData, estimates: List[Option[Time]] = Nil, isFront: Boolean = true) {
    def isBack: Boolean = !isFront

    override def toString: String = s"""Card {$data, ${if (isFront) "front" else "back"}, [${estimates.map(e => e.getOrElse("-")).mkString(", ")}]}"""
  }

  trait CardData

  case class Deck(stock: List[Card], estimated: List[Card] = Nil, discard: List[Card] = Nil) {
    override def toString: String = {
      "Deck {" +
        "\n  stock:\n    " + stock.mkString("\n    ") +
        "\n  estimated:\n    " + estimated.mkString("\n    ") +
        "\n  discard:\n    " + discard.mkString("\n    ") + "\n}"
    }
  }

  implicit def intTimeToOption(time: Int): Option[Time] = longTimeToOption(time)

  implicit def longTimeToOption(time: Long): Option[Time] = if (time >= 0) Some(time) else None

  implicit class CardExtensions(c: Card) {
    def backSide: Card = if (c.isFront) c.copy(isFront = false) else c

    def frontSide: Card = if (c.isFront) c else c.copy(isFront = true)

    def reverseSide: Card = c.copy(isFront = !c.isFront)

    def estimate(time: Option[Time] = None): Card = c.copy(estimates = c.estimates :+ time)
  }

  implicit class DeckExtensions(d: Deck) {

    def replaceCurrent(c: Card): Deck = d.copy(stock = c :: d.stock.tail)

    def backSideCurrent: Option[Deck] = current.map(c => d.replaceCurrent(c.backSide))

    def frontSideCurrent: Option[Deck] = current.map(c => d.replaceCurrent(c.frontSide))

    def reverseSideCurrent: Option[Deck] = current.map(c => d.replaceCurrent(c.reverseSide))

    def current: Option[Card] = d.stock.headOption

    def discardCurrent: Option[Deck] = current.map { c =>
      d.copy(stock = d.stock.tail, discard = c :: d.discard)
    }

    def estimateCurrent(time: Option[Time] = None)(implicit passes: Int): Option[Deck] = current.map { c =>
      val estimated = c.estimate(time)
      if (estimated.estimates.count(_.nonEmpty) < passes) {
        // move to estimated
        d.copy(stock = d.stock.tail, estimated = d.estimated :+ estimated)
      } else {
        // move to discard
        d.copy(stock = d.stock.tail, discard = d.discard :+ estimated)
      }
    }

    def isEmpty: Boolean = d.stock.isEmpty && d.estimated.isEmpty && d.discard.isEmpty

    def nonEmpty: Boolean = !isEmpty

    def hasCompletedPass: Boolean = d.stock.isEmpty

    def hasCompletedExam: Boolean = d.hasCompletedPass && d.estimated.isEmpty

    def newPass(implicit shuffle: Boolean = false): Option[Deck] = if (nonEmpty && !hasCompletedExam && hasCompletedPass) {
      val stock = d.estimated.map(c => c.copy(isFront = true))
      val shuffled = if (shuffle) Random.shuffle(stock) else stock

      Some(d.copy(stock = shuffled, estimated = Nil))
    } else None
  }

}

object Exam extends Exam