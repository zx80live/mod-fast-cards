package com.zx80live.mod.fastcards.util

object CollectionUtils {

  implicit class ListExtensions[T](xs: List[T]) {
    def fill(symbol: T, max: Int): List[T] = {
      var gs: List[T] = Nil
      if (xs.length < max) {
        for (i <- 0 until max - xs.length) {
          gs = symbol :: gs
        }
        xs ::: gs
      } else {
        xs
      }
    }
  }

}
