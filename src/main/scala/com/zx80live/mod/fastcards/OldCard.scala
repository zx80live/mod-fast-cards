package com.zx80live.mod.fastcards

@deprecated
case class OldCard(value: String, kind: Option[String] = None, translations: List[String], examples: List[OldExample] = Nil, transcript: Option[String] = None, var passCount: Int = 0, var times: List[Long] = Nil)

@deprecated
case class OldExample(text: String, translations: List[String] = Nil)