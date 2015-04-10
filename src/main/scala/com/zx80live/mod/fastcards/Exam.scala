package com.zx80live.mod.fastcards

import scala.util.Random

trait Exam {

  type Time = Long

  case class Card(data: CardData, estimates: List[Option[Time]] = Nil, isFront: Boolean = true) {
    def isBack: Boolean = !isFront
  }

  case class CardData()

  case class Deck(stock: List[Card], estimated: List[Card] = Nil, discard: List[Card] = Nil)

  implicit class CardExtensions(c: Card) {
    def backSide: Card = if (c.isFront) c.copy(isFront = false) else c

    def frontSide: Card = if (c.isFront) c else c.copy(isFront = true)

    def reverseSide: Card = c.copy(isFront = !c.isFront)

    def estimate(time: Option[Time] = None): Card = c.copy(estimates = time :: c.estimates)
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
        // estimate
        d.copy(stock = d.stock.tail, estimated = estimated :: d.estimated)
      } else {
        // discard
        d.copy(stock = d.stock.tail, estimated = estimated :: d.discard)
      }
    }

    def hasCompletedPass: Boolean = d.stock.isEmpty

    def newPass(implicit shuffle: Boolean = false): Option[Deck] = if (d.hasCompletedPass) {
      Some(if (shuffle) d.copy(stock = Random.shuffle(d.estimated)) else d.copy(stock = d.estimated))
    } else None
  }

}
