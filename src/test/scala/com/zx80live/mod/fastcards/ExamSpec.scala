package com.zx80live.mod.fastcards

import org.scalatest._

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
        c0.isFront shouldEqual true
        c0.isBack shouldEqual false

        val front: Exam.Card = c0.frontSide
        front shouldEqual c0

        c0.backSide.frontSide shouldEqual c0
      }

      "reverseSide" in {
        c0 shouldEqual c0.reverseSide.reverseSide

        val back: Exam.Card = c0.backSide

        back.isBack shouldEqual true
        back.isFront shouldEqual false
        back.data shouldEqual c0.data
        back.estimates shouldEqual c0.estimates

        back.reverseSide shouldEqual c0
      }

      "estimate" in {
        c0.estimates shouldEqual Nil

        // test immutability
        c0.estimate().isBack shouldEqual c0.isBack
        c0.estimate().isFront shouldEqual c0.isFront
        c0.estimate().data shouldEqual c0.data

        c0.estimate().estimates shouldEqual List(None)
        c0.estimate(1000).estimates shouldEqual List(Some(1000))
        c0.estimate(1000).estimate().estimate(2000).estimates shouldEqual List(Some(1000), None, Some(2000))
      }
    }

    "DeckExtensions" should {
      "current" in {
        // for empty stock
        Deck(Nil).current shouldBe None
        deck.current shouldBe Some(c0)
      }

      "replaceCurrent" in {
        val replacement: Exam.Card = Card(Data(10))
        val replaced: Exam.Deck = deck.replaceCurrent(replacement)

        replaced.estimated shouldEqual deck.estimated
        replaced.discard shouldEqual deck.discard

        replaced.stock shouldEqual List(replacement, c1, c2, c3)
        replaced.current shouldEqual Some(replacement)
      }

      "backSideCurrent" in {

        deck.current.get.isBack shouldEqual false
        deck.current.get shouldEqual c0

        val deck2: Option[Exam.Deck] = deck.backSideCurrent
        deck2 should not be empty
        deck2.get.stock.tail shouldEqual deck.stock.tail
        deck2.get.estimated shouldEqual deck.estimated
        deck2.get.discard shouldEqual deck.discard
        deck2.get.current.get.isBack shouldEqual true


        //for empty stock
        Deck(Nil).backSideCurrent shouldEqual None
      }

      "frontSideCurrent" in {
        deck.current.get.isFront shouldEqual true
        deck.current.get shouldEqual c0

        val deck2: Option[Exam.Deck] = deck.backSideCurrent.get.frontSideCurrent
        deck2.get shouldEqual deck
      }

      "reverseSideCurrent" in {
        deck shouldEqual deck.reverseSideCurrent.get.reverseSideCurrent.get
        deck.backSideCurrent.get shouldEqual deck.reverseSideCurrent.get
      }

      "discardCurrent" in {
        // for empty stock
        Deck(Nil).discardCurrent shouldBe None

        val deck2: Option[Exam.Deck] = deck.discardCurrent
        deck2 should not be empty
        deck2.get.current shouldEqual Some(c1)
        deck2.get.stock shouldEqual deck.stock.tail
        deck2.get.discard shouldEqual deck.stock.head :: deck.discard
      }

      "estimateCurrent" in {
        ???
      }
    }
  }
}
