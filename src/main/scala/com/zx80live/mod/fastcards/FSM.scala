package com.zx80live.mod.fastcards

trait FSM {

  trait Card

  case class Deck(stock: List[Card], discard: List[Card]) {
    def isEmptyStock: Boolean = stock.isEmpty
  }

  sealed trait FrontSide {
    this: Card =>
  }

  sealed trait BackSide {
    this: Card =>
  }

}
