package com.zx80live.mod.fastcards

import org.scalatest.{Matchers, _}


class ExamFSMSpec extends WordSpec with Matchers {

  import ExamFSM._

  val c0 = Card(Data(value = "v0", translations = List("t0")))
  val c1 = Card(Data(value = "v1", translations = List("t1")))
  val c2 = Card(Data(value = "v2", translations = List("t2")))
  val c3 = Card(Data(value = "v3", translations = List("t3")))
  val c4 = Card(Data(value = "v4", translations = List("t4")))

  val badC0 = c0.copy(times = List(None, None, None))
  val badC1 = c1.copy(times = List(None, Some(1000), Some(1000)))
  val midC2 = c2.copy(times = List(Some(1000), Some(5000), Some(4000)))
  val bestC3 = c3.copy(times = List(Some(500), Some(1000)))
  val bestC4 = c4.copy(times = List(Some(100), Some(700)))

  "ExamFSM" when {
    "base functional" should {
      "navigate.one.next" in {
        val s0 = State(stock = List(c0))
        val s1 = s0.next
        val s2 = s1.next

        s1 shouldEqual State(stock = List(c0))
        s2 shouldEqual State(stock = List(c0))

        s1.isInstanceOf[EmptyStock] shouldEqual false
        s2.isInstanceOf[EmptyStock] shouldEqual false

        s1.discard shouldEqual Nil
        s2.discard shouldEqual Nil
      }

      "navigate.one.prev" in {
        val s0 = State(stock = List(c0))
        val s1 = s0.prev
        val s2 = s1.prev

        s1 shouldEqual State(stock = List(c0))
        s2 shouldEqual State(stock = List(c0))

        s1.isInstanceOf[EmptyStock] shouldEqual false
        s2.isInstanceOf[EmptyStock] shouldEqual false

        s1.discard shouldEqual Nil
        s2.discard shouldEqual Nil
      }

      "navigate.nonEmpty.next" in {
        val s0 = State(stock = List(c0, c1, c2, c3, c4))
        val s1: ExamFSM.State = s0.next
        val s2: ExamFSM.State = s1.next
        val s3: ExamFSM.State = s2.next
        val s4: ExamFSM.State = s3.next
        val s5: ExamFSM.State = s4.next
        val s6: ExamFSM.State = s5.next

        s0.isInstanceOf[EmptyStock] shouldEqual false
        s1.isInstanceOf[EmptyStock] shouldEqual false
        s2.isInstanceOf[EmptyStock] shouldEqual false
        s3.isInstanceOf[EmptyStock] shouldEqual false
        s4.isInstanceOf[EmptyStock] shouldEqual false
        s5.isInstanceOf[EmptyStock] shouldEqual false
        s6.isInstanceOf[EmptyStock] shouldEqual false

        s0.discard shouldEqual Nil
        s1.discard shouldEqual Nil
        s2.discard shouldEqual Nil
        s3.discard shouldEqual Nil
        s4.discard shouldEqual Nil
        s5.discard shouldEqual Nil
        s6.discard shouldEqual Nil

        s1 shouldEqual State(stock = List(c1, c2, c3, c4, c0))
        s2 shouldEqual State(stock = List(c2, c3, c4, c0, c1))
        s3 shouldEqual State(stock = List(c3, c4, c0, c1, c2))
        s5 shouldEqual State(stock = List(c0, c1, c2, c3, c4))
        s6 shouldEqual State(stock = List(c1, c2, c3, c4, c0))
        s4 shouldEqual State(stock = List(c4, c0, c1, c2, c3))
      }

      "navigate.nonEmpty.prev" in {
        val s0 = State(stock = List(c0, c1, c2, c3, c4))
        val s1: ExamFSM.State = s0.prev
        val s2: ExamFSM.State = s1.prev
        val s3: ExamFSM.State = s2.prev
        val s4: ExamFSM.State = s3.prev
        val s5: ExamFSM.State = s4.prev
        val s6: ExamFSM.State = s5.prev

        s0.isInstanceOf[EmptyStock] shouldEqual false
        s1.isInstanceOf[EmptyStock] shouldEqual false
        s2.isInstanceOf[EmptyStock] shouldEqual false
        s3.isInstanceOf[EmptyStock] shouldEqual false
        s4.isInstanceOf[EmptyStock] shouldEqual false
        s5.isInstanceOf[EmptyStock] shouldEqual false
        s6.isInstanceOf[EmptyStock] shouldEqual false

        s0.discard shouldEqual Nil
        s1.discard shouldEqual Nil
        s2.discard shouldEqual Nil
        s3.discard shouldEqual Nil
        s4.discard shouldEqual Nil
        s5.discard shouldEqual Nil
        s6.discard shouldEqual Nil

        s1 shouldEqual State(stock = List(c4, c0, c1, c2, c3))
        s2 shouldEqual State(stock = List(c3, c4, c0, c1, c2))
        s3 shouldEqual State(stock = List(c2, c3, c4, c0, c1))
        s4 shouldEqual State(stock = List(c1, c2, c3, c4, c0))
        s5 shouldEqual State(stock = List(c0, c1, c2, c3, c4))
        s6 shouldEqual State(stock = List(c4, c0, c1, c2, c3))
      }


      "navigate.empty.next" in {
        val s0 = State(stock = Nil)
        val s1 = s0.next
        val s2 = s1.next

        s1 shouldEqual State(stock = Nil)
        s2 shouldEqual State(stock = Nil)

        s0.isInstanceOf[EmptyStock] shouldEqual false
        s1.isInstanceOf[EmptyStock] shouldEqual true
        s2.isInstanceOf[EmptyStock] shouldEqual true

        s1.discard shouldEqual Nil
        s2.discard shouldEqual Nil
      }

      "navigate.empty.prev" in {
        val s0 = State(stock = Nil)
        val s1 = s0.prev
        val s2 = s1.prev

        s1 shouldEqual State(stock = Nil)
        s2 shouldEqual State(stock = Nil)

        s0.isInstanceOf[EmptyStock] shouldEqual false
        s1.isInstanceOf[EmptyStock] shouldEqual true
        s2.isInstanceOf[EmptyStock] shouldEqual true

        s1.discard shouldEqual Nil
        s2.discard shouldEqual Nil
      }

      "asEmptyStock" in {
        State(stock = Nil).asEmptyStock.isInstanceOf[EmptyStock] shouldEqual true
        State(stock = List(c0)).asEmptyStock.isInstanceOf[EmptyStock] shouldEqual false
      }

      "current" in {
        State(stock = Nil).current shouldEqual None
        State(stock = List(c0)).current shouldEqual Some(c0)
        State(stock = List(c0, c1, c2)).current shouldEqual Some(c0)
      }

      "replaceCurrent" in {
        State(stock = Nil).replaceCurrent(c0) shouldEqual State(Nil, Nil)
        State(stock = List(c0)).replaceCurrent(c1) shouldEqual State(List(c1), Nil)
        State(stock = List(c0, c1, c2)).replaceCurrent(c3) shouldEqual State(List(c3, c1, c2), Nil)
      }

      "drop on empty" in {
        val s0: ExamFSM.State = State(stock = Nil, discard = Nil).drop
        s0 shouldEqual State(stock = Nil, discard = Nil)
        s0.isInstanceOf[EmptyStock] shouldEqual true
      }

      "drop on one" in {
        val s1 = State(stock = List(c0), discard = Nil).drop
        s1 shouldEqual State(stock = Nil, discard = List(c0))
        s1.isInstanceOf[EmptyStock] shouldEqual true
      }

      "drop on nonempty with empty drop" in {
        val s1 = State(stock = List(c0, c1, c2), discard = Nil).drop
        s1 shouldEqual State(stock = List(c1, c2), discard = List(c0))
        s1.isInstanceOf[EmptyStock] shouldEqual false

        val s2 = s1.drop
        s2 shouldEqual State(stock = List(c2), discard = List(c1, c0))
        s2.isInstanceOf[EmptyStock] shouldEqual false

        val s3 = s2.drop
        s3 shouldEqual State(stock = Nil, discard = List(c2, c1, c0))
        s3.isInstanceOf[EmptyStock] shouldEqual true

        val s4 = s3.drop
        s4 shouldEqual State(stock = Nil, discard = List(c2, c1, c0))
        s4.isInstanceOf[EmptyStock] shouldEqual true
      }

      "change state" in {
        val s0 = State(Nil, Nil).asEmptyStock
        s0.isInstanceOf[EmptyStock] shouldEqual true
        s0.copy(stock = List(c0)).isInstanceOf[EmptyStock] shouldEqual false
        s0.copy(stock = List(c0)).asEmptyStock.isInstanceOf[EmptyStock] shouldEqual false
      }

      "deck" in {
        State(Nil, Nil).deck shouldEqual Nil
        State(Nil, List(c0)).deck shouldEqual List(c0)
        State(Nil, List(c0, c1)).deck shouldEqual List(c0, c1)
        State(List(c0), Nil).deck shouldEqual List(c0)
        State(List(c0, c1), Nil).deck shouldEqual List(c0, c1)
        State(List(c0), List(c1, c2)).deck shouldEqual List(c0, c1, c2)
        State(List(c0, c1), List(c2)).deck shouldEqual List(c0, c1, c2)
      }
    }

    "Card extensions" should {
      "addPass" in {
        c0.times shouldEqual Nil
        c0.addPass().times shouldEqual List(None)
        c0.addPass(Some(1000)).addPass().addPass(None).addPass(Some(2000)).times shouldEqual List(Some(1000), None, None, Some(2000))
      }

      "averagePassTime with all true passes" in {
        val cardWithPasses = c0.addPass(Some(1000)).addPass(Some(2000)).addPass(Some(3000))
        val realTimes: List[Long] = cardWithPasses.times.flatten
        cardWithPasses.averagePassTime shouldEqual Some(realTimes.sum / realTimes.length)
      }

      "averagePassTime with true and fail passes" in {
        val cardWithPasses = c0.addPass(Some(1000)).addPass().addPass(Some(2000)).addPass().addPass(Some(3000))
        val realTimes: List[Long] = cardWithPasses.times.map(_.getOrElse(BAD_TIME_IN_MS))
        cardWithPasses.averagePassTime shouldEqual Some(realTimes.sum / realTimes.length)
      }

      "isExamCompleted" in {
        implicit val passCount: Int = 2
        c0.isExamCompleted shouldEqual false
        c0.addPass(Some(1000)).isExamCompleted shouldEqual false
        c0.addPass(Some(1000)).addPass(Some(2000)).addPass(Some(3000)).isExamCompleted shouldEqual true
        c0.addPass(Some(1000)).addPass(Some(2000)).isExamCompleted shouldEqual true
        c0.addPass(Some(1000)).addPass(Some(2000)).addPass().isExamCompleted shouldEqual false
        c0.addPass(Some(1000)).addPass(Some(2000)).addPass().addPass(Some(1000)).isExamCompleted shouldEqual false
        c0.addPass(Some(1000)).addPass(Some(2000)).addPass().addPass(Some(1000)).addPass(Some(2000)).isExamCompleted shouldEqual true
        c0.addPass(Some(1000)).addPass(Some(2000)).addPass().addPass(Some(1000)).addPass(Some(2000)).addPass(Some(3000)).isExamCompleted shouldEqual true
      }
    }

    "extend functional" should {
      "estimateFalse" in {
        val s0 = State(stock = List(c0, c1, c2, c3))
        val s1: ExamFSM.State = s0.estimateFalse
        s1.stock.length shouldEqual s0.stock.length
        s1.discard shouldEqual Nil
        s1.stock.last shouldEqual Card(c0.data, List(None))
        s1 shouldEqual State(stock = List(c1, c2, c3, Card(c0.data, List(None))))

        val s2: ExamFSM.State = s1.estimateFalse.estimateFalse.estimateFalse.estimateFalse
        s2.stock.length shouldEqual s0.stock.length
        s2.stock.last shouldEqual Card(c0.data, List(None, None))
        s2.discard shouldEqual Nil
      }

      "estimateTrue with all true passes" in {
        implicit val passCount: Int = 3

        val s0 = State(stock = List(c0, c1, c2))
        val s1: ExamFSM.State = s0.estimateTrue(1000)
        s1.stock.length shouldEqual s0.stock.length
        s1.discard.length shouldEqual s0.discard.length
        s1.stock.last shouldEqual Card(c0.data, List(Some(1000)))

        val s2: ExamFSM.State = s1.estimateTrue(1000).estimateTrue(1000).estimateTrue(2000)
        s2.stock.length shouldEqual s0.stock.length
        s2.discard.length shouldEqual s0.discard.length
        s2.stock.last shouldEqual Card(c0.data, List(Some(1000), Some(2000)))

        val s3: ExamFSM.State = s2.estimateTrue(1000).estimateTrue(1000).estimateTrue(3000)
        s3.stock.length shouldEqual s0.stock.length - 1
        s3.discard.length shouldEqual s0.discard.length + 1

        s3.stock shouldEqual List(Card(c1.data, List(Some(1000), Some(1000))), Card(c2.data, List(Some(1000), Some(1000))))
        s3.discard shouldEqual List(Card(c0.data, List(Some(1000), Some(2000), Some(3000))))
      }

      "estimateTrue with reset true passes" in {
        implicit val passCount: Int = 3
        val s0 = State(stock = List(c0, c1, c2))
        val s1: ExamFSM.State = s0.estimateTrue(1000)
        s1.stock.length shouldEqual s0.stock.length
        s1.discard.length shouldEqual s0.discard.length
        s1.stock.last shouldEqual Card(c0.data, List(Some(1000)))

        val s2: ExamFSM.State = s1.estimateTrue(1000).estimateTrue(1000).estimateTrue(2000)
        val s3: ExamFSM.State = s2.estimateTrue(1000).estimateTrue(1000).estimateFalse
        s3.stock.length shouldEqual s0.stock.length
        s3.discard.length shouldEqual s0.discard.length

        s3.stock shouldEqual List(Card(c1.data, List(Some(1000), Some(1000))), Card(c2.data, List(Some(1000), Some(1000))), Card(c0.data, List(Some(1000), Some(2000), None)))
        s3.discard shouldEqual Nil
      }

      "estimateTrue with true passes and empty stock" in {
        implicit val passCount: Int = 2
        val s0 = State(stock = List(c0, c1, c2))


        val s1 = s0.estimateTrue(1000).estimateTrue(1000).estimateTrue(1000).estimateTrue(1000) // c1, c2, -> drop: c0
        s1.isInstanceOf[EmptyStock] shouldEqual false
        s1.stock.length shouldEqual s0.stock.length - 1
        s1.discard.length shouldEqual s0.discard.length + 1

        val s2 = s1.estimateTrue(1000) // c2, -> drop: c1
        s2.isInstanceOf[EmptyStock] shouldEqual false
        s2.stock.length shouldEqual s1.stock.length - 1
        s2.discard.length shouldEqual s1.discard.length + 1

        val s3 = s2.estimateTrue(1000).estimateTrue(1000) // -> drop: c2
        s3.stock.length shouldEqual s2.stock.length - 1
        s3.discard.length shouldEqual s2.discard.length + 1
        s3.isInstanceOf[EmptyStock] shouldEqual true

        val s4: ExamFSM.State = s3.estimateTrue(1000)
        s4.isInstanceOf[EmptyStock] shouldEqual true
        s4 shouldEqual s3

        val s5: ExamFSM.State = s3.estimateFalse
        s5.isInstanceOf[EmptyStock] shouldEqual true
        s5 shouldEqual s3
      }

      "badCards" in {
        State(List(badC0, badC1, midC2, bestC3, bestC4)).badCards shouldEqual List(badC0, badC1)
      }

      "middleCards" in {
        State(List(badC0, badC1, midC2, bestC3, bestC4)).middleCards shouldEqual List(midC2)
      }

      "bestCards" in {
        State(List(badC0, badC1, midC2, bestC3, bestC4)).bestCards shouldEqual List(bestC3, bestC4)
      }


    }
  }


}
