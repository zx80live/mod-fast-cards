package com.zx80live.mod.fastcards

case class Card(value: String, kind: Option[String] = None, translations: List[String], examples: List[Example] = Nil, transcript: Option[String] = None, var passCount: Int = 0, var times: List[Long] = Nil)

case class Example(text: String, translation: List[String] = Nil)