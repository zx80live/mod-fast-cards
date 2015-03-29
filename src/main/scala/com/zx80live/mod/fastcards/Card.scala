package com.zx80live.mod.fastcards

case class Card(data: Data, times: List[Option[Long]] = Nil)

case class Data(value: String,
                kind: Option[String] = None,
                translations: List[String],
                examples: List[Example] = Nil,
                transcript: Option[String] = None)


case class Example(text: String, translations: List[String] = Nil)

case class Deck(stock: List[Card], discard: List[Card] = Nil, answered: List[Card] = Nil)

case class Statistic(best: List[Card], middle: List[Card], bad: List[Card])
