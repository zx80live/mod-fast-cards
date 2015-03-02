package com.zx80live.mod.fastcards

case class Card(value: String, kind: Option[String] = None, translations: List[String], examples: List[Example] = Nil, var statistic: Int = 0)

case class Example(text: String, translation: List[String] = Nil)