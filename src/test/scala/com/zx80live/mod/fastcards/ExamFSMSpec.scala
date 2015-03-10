package com.zx80live.mod.fastcards

import org.scalatest.{Matchers, _}


class ExamFSMSpec extends WordSpec with Matchers {

  import ExamFSM._

  val c0 = Card(Data(value = "v0", translations = List("t0")))
  val c1 = Card(Data(value = "v1", translations = List("t1")))
  val c2 = Card(Data(value = "v2", translations = List("t2")))
  val c3 = Card(Data(value = "v3", translations = List("t3")))
  val c4 = Card(Data(value = "v4", translations = List("t4")))

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

      "truePassesCount" in {
        c0.addPass(Some(1000)).addPass(Some(2000)).addPass(Some(3000)).truePassesCount shouldEqual 3
        c0.addPass(Some(1000)).addPass().addPass(Some(2000)).addPass().addPass(Some(3000)).truePassesCount shouldEqual 3
      }
    }

    "extend functional" should {
      "estimateFalse" in {}

      "estimateTrue" in {}
    }
  }


}
