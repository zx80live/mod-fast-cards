package com.zx80live.mod.fastcards

/**
 * Finite State Machine for exam
 */
trait ExamFSM {

  case class Card(data: Data, times: List[Option[Long]] = Nil)

  case class Data(value: String,
                  kind: Option[String] = None,
                  translations: List[String],
                  examples: List[Example] = Nil,
                  transcript: Option[String] = None)


  case class Example(text: String, translations: List[String] = Nil)

  case class State(stock: List[Card], discard: List[Card] = Nil)

  case class Statistic(best: List[Card], middle: List[Card], bad: List[Card])

  sealed trait EmptyStock

  object EmptyStock extends EmptyStock

  val BAD_TIME_IN_MS: Long = 1000 * 60L
  val limitBadMs: Long = 8000
  val limitBestMs: Long = 3000

  implicit class CardExtensions(c: Card) {
    def addPass(p: Option[Long] = None): Card = c.copy(times = c.times :+ p)

    def averagePassTime: Option[Double] = if (c.times.nonEmpty) {
      val times: List[Long] = c.times.map(t => t.getOrElse(BAD_TIME_IN_MS))
      Some(times.sum / times.length)
    } else None

    def isExamCompleted(implicit passCount: Int): Boolean = {
      if (c.times.length >= passCount) {
        c.times.takeRight(passCount).count(_.isDefined) >= passCount
      } else {
        false
      }
    }
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

    def estimateTrue(time: Long)(implicit truePassLimit: Int): State =
      s.stock.headOption.map { head =>
        val c = head.addPass(Some(time))

        (c.isExamCompleted, s.replaceCurrent(c)) match {
          case (true, state) => state.drop
          case (false, state) => state.next
        }
      }.getOrElse(s).asEmptyStock

    def estimateFalse: State = s.stock.headOption.map { c =>
      s.replaceCurrent(c.addPass()).next
    }.getOrElse(s).asEmptyStock

    def deck: List[Card] = s.stock ::: s.discard

    def bestCards: List[Card] = deck.filter(c => c.averagePassTime.getOrElse(BAD_TIME_IN_MS.toDouble) < limitBestMs)

    def middleCards: List[Card] = deck.filter(c => {
      val avg = c.averagePassTime.getOrElse(BAD_TIME_IN_MS.toDouble)
      avg >= limitBestMs && avg <= limitBadMs
    })

    def badCards: List[Card] = deck.filter(c => c.averagePassTime.getOrElse(BAD_TIME_IN_MS.toDouble) > limitBadMs)

    def statistic: Statistic = Statistic(bestCards, middleCards, badCards)
  }

}

object ExamFSM extends ExamFSM
