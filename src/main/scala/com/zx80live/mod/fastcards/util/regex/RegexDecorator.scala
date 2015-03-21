package com.zx80live.mod.fastcards.util.regex

trait RegexDecorator {

  implicit class StringDecorateExtensions(s: String) {
    def decorateAll(pattern: String, left: String, right: String): String = s
  }

}

object RegexDecorator extends RegexDecorator
