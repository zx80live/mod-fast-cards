package com.zx80live.mod.fastcards

@deprecated
case class Card(value: String, kind: Option[String] = None, translations: List[String], examples: List[Example] = Nil, transcript: Option[String] = None, var passCount: Int = 0, var times: List[Long] = Nil)

@deprecated
case class Example(text: String, translations: List[String] = Nil)