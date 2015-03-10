package com.zx80live.mod.fastcards

import org.scalatest.{Matchers, _}


class ExamFSMSpec extends FlatSpec with Matchers {

  import ExamFSM._

  val c0 = Card(Data(value = "v0", translations = List("t0")))
  val c1 = Card(Data(value = "v1", translations = List("t1")))
  val c2 = Card(Data(value = "v2", translations = List("t2")))
  val c3 = Card(Data(value = "v3", translations = List("t3")))
  val c4 = Card(Data(value = "v4", translations = List("t4")))


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


}
