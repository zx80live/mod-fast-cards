package com.zx80live.mod.fastcards

import org.scalatest._

class ExamSpec extends WordSpec with Matchers {

  import Exam._

  case class Data(id: Int) extends Exam.CardData

  val c0 = Card(Data(0))
  val c1 = Card(Data(1))
  val c2 = Card(Data(2))
  val c3 = Card(Data(3))

  val c0estimated = Card(c0.data, List(None))
  val c1estimated = Card(c1.data, List(Some(1000)))
  val c2estimated = Card(c2.data, List(Some(2000)))
  val c3estimated = Card(c3.data, List(Some(3000)))

  val deck = Deck(List(c0, c1, c2, c3))
  val someCards = List(Card(Data(100)), Card(Data(200)), Card(Data(300)))
  val deckWithCompletedPassWithEmptyDiscard = Deck(Nil, List(c0, c1, c2, c3), Nil)
  val deckWithCompletedPassWithDiscard = Deck(Nil, List(c0, c1), List(c2, c3))
  val deckWithCompletedExam = Deck(Nil, Nil, List(c0, c1, c2, c3))

  val deckWithOnePass: Exam.Deck = Deck(List(c0estimated, c1estimated, c2estimated, c3estimated))


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

      "hasCompletedPass" in {
        deck.hasCompletedPass shouldEqual false
        Deck(Nil, Nil, Nil).hasCompletedPass shouldEqual true
        Deck(someCards, Nil, Nil).hasCompletedPass shouldEqual false
        Deck(someCards, someCards, Nil).hasCompletedPass shouldEqual false
        Deck(someCards, Nil, someCards).hasCompletedPass shouldEqual false
        Deck(Nil, someCards, someCards).hasCompletedPass shouldEqual true
        Deck(Nil, Nil, someCards).hasCompletedPass shouldEqual true
      }

      "hasCompleteExam" in {
        deck.hasCompletedExam shouldEqual false
        Deck(Nil, Nil, Nil).hasCompletedExam shouldEqual true
        Deck(someCards, Nil, Nil).hasCompletedExam shouldEqual false
        Deck(someCards, someCards, Nil).hasCompletedExam shouldEqual false
        Deck(someCards, Nil, someCards).hasCompletedExam shouldEqual false
        Deck(Nil, someCards, someCards).hasCompletedExam shouldEqual false
        Deck(Nil, Nil, someCards).hasCompletedExam shouldEqual true
      }

      "isEmpty" in {
        Deck(Nil).isEmpty shouldEqual true
        Deck(Nil, Nil, Nil).isEmpty shouldEqual true
        Deck(someCards, Nil, Nil).isEmpty shouldEqual false
        Deck(Nil, someCards, Nil).isEmpty shouldEqual false
        Deck(someCards, someCards, Nil).isEmpty shouldEqual false
        Deck(Nil, Nil, someCards).isEmpty shouldEqual false
        Deck(someCards, Nil, someCards).isEmpty shouldEqual false
        Deck(Nil, someCards, someCards).isEmpty shouldEqual false
        Deck(someCards, someCards, someCards).isEmpty shouldEqual false
      }

      "nonEmpty" in {
        Deck(Nil).nonEmpty shouldEqual false
        Deck(Nil, Nil, Nil).nonEmpty shouldEqual false
        Deck(someCards, Nil, Nil).nonEmpty shouldEqual true
        Deck(Nil, someCards, Nil).nonEmpty shouldEqual true
        Deck(someCards, someCards, Nil).nonEmpty shouldEqual true
        Deck(Nil, Nil, someCards).nonEmpty shouldEqual true
        Deck(someCards, Nil, someCards).nonEmpty shouldEqual true
        Deck(Nil, someCards, someCards).nonEmpty shouldEqual true
        Deck(someCards, someCards, someCards).nonEmpty shouldEqual true
      }

      "estimateCurrent" in {
        implicit val passes: Int = 2

        deck.current.get.estimates shouldEqual Nil

        // once estimate
        deck.estimateCurrent() should not be empty
        deck.estimateCurrent().get.current shouldBe Some(c1)
        deck.estimateCurrent().get.estimated shouldEqual deck.current.get.estimate() :: deck.estimated
        deck.estimateCurrent().get.discard shouldEqual deck.discard

        deck.estimateCurrent(1000) should not be empty
        deck.estimateCurrent(1000).get.current shouldBe Some(c1)
        deck.estimateCurrent(1000).get.estimated shouldEqual deck.current.get.estimate(1000) :: deck.estimated
        deck.estimateCurrent(1000).get.discard shouldEqual deck.discard

        // a lot of estimates for complete pass
        val d0: Option[Exam.Deck] = deck.estimateCurrent()
        d0 should not be empty
        d0.get.hasCompletedPass shouldEqual false
        val d1: Option[Exam.Deck] = d0.get.estimateCurrent(1000)
        d1 should not be empty
        d1.get.hasCompletedPass shouldEqual false
        val d2: Option[Exam.Deck] = d1.get.estimateCurrent(2000)
        d2 should not be empty
        d2.get.hasCompletedPass shouldEqual false
        val d3: Option[Exam.Deck] = d2.get.estimateCurrent(3000)
        d3 should not be empty
        d3.get.hasCompletedPass shouldEqual true
        d3.get.estimateCurrent() shouldEqual None

        d3.get.stock shouldEqual Nil
        d3.get.estimated.map(c => (c.data, c.isBack)) shouldEqual deck.stock.map(c => (c.data, c.isBack))
        d3.get.discard shouldEqual deck.discard


      }

      "estimateCurrent: estimate true for trued estimated card" in {
        implicit val passes:Int = 1
        val d = deckWithOnePass.discardCurrent.get
        d.hasCompletedExam shouldEqual false
        d.hasCompletedPass shouldEqual false

        println(d)

        val estimated: Option[Exam.Deck] = d.estimateCurrent(1000)
        estimated should not be empty
        estimated.get.discard shouldEqual (d.discard :+ d.current.get.estimate(1000))
        estimated.get.estimated shouldEqual d.estimated
      }

//      "estimateCurrent: estimate true for false estimated card" in {
//        implicit val passes:Int = 2
//        val d = deckWithOnePass
//        d.hasCompletedExam shouldEqual false
//        d.hasCompletedPass shouldEqual false
//        ???
//      }
//
//      "estimateCurrent: estimate false for true estimated card" in {
//        implicit val passes:Int = 2
//        val d = deckWithOnePass
//        d.hasCompletedExam shouldEqual false
//        d.hasCompletedPass shouldEqual false
//        ???
//      }
//
//      "estimateCurrent: estimate false for false estimated card" in {
//        implicit val passes:Int = 2
//        val d = deckWithOnePass
//        d.hasCompletedExam shouldEqual false
//        d.hasCompletedPass shouldEqual false
//        ???
//      }

      "newPass" in {
        // for empty deck
        Deck(Nil).newPass shouldEqual None

        // for non-completed pass
        deck.hasCompletedPass shouldEqual false
        deck.hasCompletedExam shouldEqual false
        deck.newPass shouldEqual None

        // for completed pass and empty discard
        deckWithCompletedPassWithEmptyDiscard.hasCompletedPass shouldEqual true
        deckWithCompletedPassWithEmptyDiscard.hasCompletedExam shouldEqual false
        val pass1: Option[Exam.Deck] = deckWithCompletedPassWithEmptyDiscard.newPass(shuffle = false)
        pass1 should not be empty
        pass1.get shouldEqual Deck(
          deckWithCompletedPassWithEmptyDiscard.estimated.map(c => c.copy(estimates = Nil, isFront = true)),
          Nil,
          deckWithCompletedPassWithEmptyDiscard.discard)

        // for completed pass and non-empty discard
        deckWithCompletedPassWithDiscard.hasCompletedPass shouldEqual true
        deckWithCompletedPassWithDiscard.hasCompletedExam shouldEqual false
        val pass2: Option[Exam.Deck] = deckWithCompletedPassWithDiscard.newPass(shuffle = false)
        pass2 should not be empty
        pass2.get shouldEqual Deck(
          deckWithCompletedPassWithDiscard.estimated.map(c => c.copy(estimates = Nil, isFront = true)),
          Nil,
          deckWithCompletedPassWithDiscard.discard)

        // for completed exam
        deckWithCompletedExam.hasCompletedPass shouldEqual true
        deckWithCompletedExam.hasCompletedExam shouldEqual true
        deckWithCompletedExam.newPass(shuffle = false) shouldEqual None
      }
    }
  }
}
