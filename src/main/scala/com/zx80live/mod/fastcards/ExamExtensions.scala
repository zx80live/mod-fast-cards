package com.zx80live.mod.fastcards

/**
 * Finite State Machine for exam
 */
trait ExamExtensions {

  sealed trait BackSide {
    this: Card =>
  }

  sealed trait InfoSide {
    this: Card =>
  }

  sealed trait EmptyStock {
    this: Deck =>
  }


  val BAD_TIME_IN_MS: Long = 1000 * 60L
  val limitBadMs: Long = 8000
  val limitBestMs: Long = 3000

  implicit class CardExtensions(c: Card) {

    //todo test
    def reset: Card = new Card(c.data, c.times)

    //todo test
    def info: Card = c match {
      case _: BackSide => new Card(c.data, c.times) with BackSide with InfoSide
      case _ => new Card(c.data, c.times) with InfoSide
    }

    def frontSide: Card = c match {
      case _: BackSide => new Card(c.data, c.times)
      case _ => c
    }

    def backSide: Card = c match {
      case _: BackSide => c
      case _ => new Card(c.data, c.times) with BackSide
    }

    //todo change test with InfoSide
    def reverseSide: Card = c match {
      case _: BackSide with InfoSide => new Card(c.data, c.times) with BackSide
      case _: BackSide => new Card(c.data, c.times)
      case _: Card with InfoSide => new Card(c.data, c.times)
      case _ => new Card(c.data, c.times) with BackSide
    }

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

  implicit class DeckExtensions(d: Deck) {

    //todo test
    def resetCurrent: Deck = d.current.map(c => d.replaceCurrent(c.reset)).getOrElse(d)

    //todo test
    def infoCurrent: Deck = d.current.map(c => d.replaceCurrent(c.info)).getOrElse(d)

    def frontSideCurrent: Deck = d.current.map(c => d.replaceCurrent(c.frontSide)).getOrElse(d)

    def backSideCurrent: Deck = d.current.map(c => d.replaceCurrent(c.backSide)).getOrElse(d)

    def reverseSideCurrent: Deck = d.current.map(c => d.replaceCurrent(c.reverseSide)).getOrElse(d)

    def asEmptyStock: Deck = if (d.stock.isEmpty) new Deck(d.stock, d.discard) with EmptyStock else d

    def current: Option[Card] = d.stock.headOption

    def replaceCurrent(c: Card): Deck = d.stock.headOption.map(h => d.copy(stock = c :: d.stock.tail)).getOrElse(d)

    def next: Deck = d.copy(stock = d.stock.headOption.map(h => d.stock.tail :+ h).getOrElse(Nil)).asEmptyStock

    def prev: Deck = d.copy(stock = d.stock.lastOption.map(l => l :: d.stock.take(d.stock.length - 1)).getOrElse(Nil)).asEmptyStock

    def drop: Deck =
      (d.stock match {
        case head :: tail =>
          d.copy(stock = tail, discard = head :: d.discard)
        case _ => d
      }).asEmptyStock

    def dropAll: Deck = d.copy(stock = Nil, discard = d.stock ::: d.discard).asEmptyStock

    def estimateTrue(time: Long)(implicit truePassLimit: Int): Deck =
      d.stock.headOption.map { head =>
        val c = head.addPass(Some(time))

        (c.isExamCompleted, d.replaceCurrent(c)) match {
          case (true, deck) => deck.drop
          case (false, deck) => deck.next
        }
      }.getOrElse(d).asEmptyStock

    def estimateFalse: Deck = d.stock.headOption.map { c =>
      d.replaceCurrent(c.addPass()).next
    }.getOrElse(d).asEmptyStock

    def estimateFalseAndDrop: Deck = d.stock.headOption.map { c =>
      d.replaceCurrent(c.addPass()).drop
    }.getOrElse(d).asEmptyStock

    def deck: List[Card] = d.stock ::: d.discard

    def bestCards: List[Card] = deck.filter(c => c.averagePassTime.getOrElse(BAD_TIME_IN_MS.toDouble) < limitBestMs)

    def middleCards: List[Card] = deck.filter(c => {
      val avg = c.averagePassTime.getOrElse(BAD_TIME_IN_MS.toDouble)
      avg >= limitBestMs && avg <= limitBadMs
    })

    def badCards: List[Card] = deck.filter(c => c.averagePassTime.getOrElse(BAD_TIME_IN_MS.toDouble) > limitBadMs)

    def statistic: Statistic = Statistic(bestCards, middleCards, badCards)
  }


}

object ExamExtensions extends ExamExtensions
