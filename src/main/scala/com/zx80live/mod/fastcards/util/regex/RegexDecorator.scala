package com.zx80live.mod.fastcards.util.regex

import java.util.regex.{Matcher, Pattern}

trait RegexDecorator {

  implicit class StringDecorateExtensions(s: String) {
    def decorateAll(pattern: String, left: String, right: String): String = {
      val p: Pattern = Pattern.compile(pattern)
      val m: Matcher = p.matcher(s)
      val sb = new StringBuffer()

      while (m.find()) {
        m.appendReplacement(sb, left + m.group(1) + right)
      }
      m.appendTail(sb)
      sb.toString
    }

  }

}

object RegexDecorator extends RegexDecorator
