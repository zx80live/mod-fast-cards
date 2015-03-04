package com.zx80live.mod.fastcards.util


import java.io.{File, FileInputStream}
import java.nio.MappedByteBuffer
import java.nio.channels.FileChannel

object NIOUtils {

  def batchedReadDelimitedStrings(file: File, delimiter: Char, batchSize: Int = 10)(batchHandler: List[String] => Unit): Unit = {
    var xs: List[String] = List()
    var size = batchSize

    readDelimitedStrings(file, delimiter) { line: String =>
      xs = line :: xs
      size = size - 1
      if (size == 0) {
        batchHandler(xs.reverse)
        xs = List()
        size = batchSize
      }
    }
    if (xs.nonEmpty) {
      batchHandler(xs.reverse)
    }
  }

  def readDelimitedStrings(file: File, delimiter: Char)(elementHandler: String => Unit): Unit = {
    import resource._

    for (is <- managed(new FileInputStream(file)); channel <- managed(is.getChannel)) {
      val b: MappedByteBuffer = channel.map(FileChannel.MapMode.READ_ONLY, 0, channel.size)
      var cursor = 0
      while (b.hasRemaining) {
        if (b.get == delimiter) {
          val length = b.position - cursor
          val bytes = Array.fill[Byte](length)(0)
          b.position(cursor)
          b.get(bytes)
          cursor = b.position
          elementHandler(new String(bytes))
        }
      }
    }
  }
}

