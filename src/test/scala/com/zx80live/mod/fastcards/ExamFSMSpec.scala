package com.zx80live.mod.fastcards

import org.scalatest.{Matchers, _}


class ExamFSMSpec extends FlatSpec with Matchers {

  import ExamFSM._

  val c0 = Card(Data(value = "v0", translations = List("t0")))
  val c1 = Card(Data(value = "v1", translations = List("t1")))
  val c2 = Card(Data(value = "v2", translations = List("t2")))
  val c3 = Card(Data(value = "v3", translations = List("t3")))
  val c4 = Card(Data(value = "v4", translations = List("t4")))

  "ExamFSM" should "navigate.one.next" in {
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

  "ExamFSM" should "navigate.one.prev" in {
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

  "ExamFSM" should "navigate.nonEmpty.next" in {
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

  "ExamFSM" should "navigate.nonEmpty.prev" in {
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


  "ExamFSM" should "navigate.empty.next" in {
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

  "ExamFSM" should "navigate.empty.prev" in {
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

  "ExamFSM" should "asEmptyStock" in {
    State(stock = Nil).asEmptyStock.isInstanceOf[EmptyStock] shouldEqual true
    State(stock = List(c0)).asEmptyStock.isInstanceOf[EmptyStock] shouldEqual false
  }

  "ExamFSM" should "current" in {
    State(stock = Nil).current shouldEqual None
    State(stock = List(c0)).current shouldEqual Some(c0)
    State(stock = List(c0, c1, c2)).current shouldEqual Some(c0)
  }

  "ExamFSM" should "replaceCurrent" in {
    State(stock = Nil).replaceCurrent(c0) shouldEqual State(Nil, Nil)
    State(stock = List(c0)).replaceCurrent(c1) shouldEqual State(List(c1), Nil)
    State(stock = List(c0, c1, c2)).replaceCurrent(c3) shouldEqual State(List(c3, c1, c2), Nil)
  }

  "ExamFSM" should "drop on empty" in {
    val s0: ExamFSM.State = State(stock = Nil, discard = Nil).drop
    s0 shouldEqual State(stock = Nil, discard = Nil)
    s0.isInstanceOf[EmptyStock] shouldEqual true
  }

  "ExamFSM" should "drop on one" in {
    val s1 = State(stock = List(c0), discard = Nil).drop
    s1 shouldEqual State(stock = Nil, discard = List(c0))
    s1.isInstanceOf[EmptyStock] shouldEqual true
  }

  "ExamFSM" should "drop on nonempty with empty drop" in {
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

  "ExamFSM" should "estimate" in {
  }
}
