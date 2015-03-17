package com.zx80live.mod.fastcards

/**
 * Finite State Machine for exam
 */
trait ExamFSM {

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

  implicit class DeckExtensions(s: Deck) {

    def asEmptyStock: Deck = if (s.stock.isEmpty) new Deck(s.stock, s.discard) with EmptyStock else s

    def current: Option[Card] = s.stock.headOption

    def replaceCurrent(c: Card): Deck = s.stock.headOption.map(h => s.copy(stock = c :: s.stock.tail)).getOrElse(s)

    def next: Deck = s.copy(stock = s.stock.headOption.map(h => s.stock.tail :+ h).getOrElse(Nil)).asEmptyStock

    def prev: Deck = s.copy(stock = s.stock.lastOption.map(l => l :: s.stock.take(s.stock.length - 1)).getOrElse(Nil)).asEmptyStock

    def drop: Deck =
      (s.stock match {
        case head :: tail =>
          s.copy(stock = tail, discard = head :: s.discard)
        case _ => s
      }).asEmptyStock

    //todo test
    def dropAll: Deck = s.copy(stock = Nil, discard = s.stock ::: s.discard).asEmptyStock

    def estimateTrue(time: Long)(implicit truePassLimit: Int): Deck =
      s.stock.headOption.map { head =>
        val c = head.addPass(Some(time))

        (c.isExamCompleted, s.replaceCurrent(c)) match {
          case (true, deck) => deck.drop
          case (false, deck) => deck.next
        }
      }.getOrElse(s).asEmptyStock

    def estimateFalse: Deck = s.stock.headOption.map { c =>
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
