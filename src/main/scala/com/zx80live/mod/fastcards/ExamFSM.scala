package com.zx80live.mod.fastcards

/**
 * Finite State Machine for exam
 */
trait ExamFSM {

  case class Card(data: Data, passes: List[Pass] = Nil)

  case class Data(value: String,
                  kind: Option[String] = None,
                  translations: List[String],
                  examples: List[Example] = Nil,
                  transcript: Option[String] = None)


  case class Pass(estimate: Boolean, time: Option[Long] = None)

  case class Example(text: String, translations: List[String] = Nil)

  case class State(stock: List[Card], discard: List[Card] = Nil)

  sealed trait EmptyStock

  implicit class CardExtensions(c: Card) {
    def addPass(p: Pass): Card = c.copy(passes = p :: c.passes)

    def averagePassTime: Option[Double] = if (c.passes.nonEmpty) {
      val times: List[Long] = c.passes.map(_.time).flatten
      Some(times.sum / times.length)
    } else None

    def truePassesCount: Int = c.passes.count(_.estimate)
  }

  implicit class StateExtensions(s: State) {

    def asEmptyStock: State = if (s.stock.isEmpty) new State(s.stock, s.discard) with EmptyStock else s

    def current: Option[Card] = s.stock.headOption

    def replaceCurrent(c: Card): State = s.stock.headOption.map(h => s.copy(stock = c :: s.stock.tail)).getOrElse(s)

    def next: State = s.copy(stock = s.stock.headOption.map(h => s.stock.tail :+ h).getOrElse(Nil)).asEmptyStock

    def prev: State = s.copy(stock = s.stock.lastOption.map(l => l :: s.stock.take(s.stock.length - 1)).getOrElse(Nil)).asEmptyStock

    def drop: State =
      (s.stock match {
        case head :: tail =>
          s.copy(stock = tail, discard = head :: s.discard)
        case _ => s
      }).asEmptyStock


    def estimate(value: Boolean, time: Option[Long] = None)(implicit truePassLimit: Int) =
      s.stock.headOption.map { head =>
        val c = head.addPass(Pass(value, time))

        (c.truePassesCount < truePassLimit, s.replaceCurrent(c)) match {
          case (true, state) => state.next
          case (false, state) => state.discard
        }
      }.getOrElse(s.asEmptyStock)
  }

}

object ExamFSM extends ExamFSM
