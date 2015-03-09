package com.zx80live.mod.fastcards

import scala.util.Random

trait ExamService {

  implicit class ImmutableCardListExt(xs: List[Card]) {

    def shuffle: List[Card] = Random.shuffle(xs)

    def next: List[Card] = xs.headOption.map(h => xs.tail :+ h).getOrElse(Nil)

    def prev: List[Card] = xs.lastOption.map(l => l :: xs.take(xs.length - 1)).getOrElse(Nil)

    def drop: (Option[Card], List[Card]) = (xs.headOption, xs.tail)


    def filterTop: List[Card] = xs.filter(e => averageTime(e.times).exists(_ <= topLimitMs))

    def filterMid: List[Card] = xs.filter { e => averageTime(e.times).exists(t => t > topLimitMs && t <= lowLimitMs)}

    def filterLow: List[Card] = xs.filter(e => averageTime(e.times).exists(_ <= lowLimitMs))

    private def averageTime(list: List[Long]): Option[Double] = if (list.nonEmpty) {
      Some(list.sum / list.length)
    } else None
  }

}
