package com.zx80live.mod.fastcards

import org.scalatest.{Matchers, WordSpec}

class ExamSpec extends WordSpec with Matchers {

  import Exam._

  case class Data(id: Int) extends Exam.CardData

  val c0 = Card(Data(0))
  val c1 = Card(Data(1))
  val c2 = Card(Data(2))
  val c3 = Card(Data(3))
  val deck = Deck(List(c0, c1, c2, c3))


  "Exam" when {
    "CardExtensions" should {
      "backSide" in {
        c0.isBack shouldEqual false
        c0.isFront shouldEqual true

        val back: Exam.Card = c0.backSide

        back.isBack shouldEqual true
        back.isFront shouldEqual false
        back.data shouldEqual c0.data
        back.estimates shouldEqual c0.estimates

        back.backSide shouldEqual back
      }

      "frontSide" in {
        ???
      }

      "reverseSide" in {
        ???
      }

      "estimate" in {
        ???
      }
    }
  }
}
