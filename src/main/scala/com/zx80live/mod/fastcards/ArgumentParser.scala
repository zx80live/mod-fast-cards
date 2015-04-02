package com.zx80live.mod.fastcards

import com.zx80live.mod.fastcards.util.CardsReader

import scala.util.{Success, Try}

trait ArgumentParser {

  import java.io.File


  case class Config(files: Seq[File] = Seq(), enRu: Boolean = false, filter: Seq[String] = Seq(), noShuffle: Boolean = false, passCount: Int = 2)

  @deprecated
  implicit class ConfigExtensions(c: Config) {
    def badMode = c.files.map(getFileExtension).collect { case Some("bad") | Some("mid") => true }.length > 0

    def badFilePrefixOpt: Option[String] = if (!badMode) Some(c.files.map(_.getName).mkString("_")) else None
  }

  implicit class FileListExtensions(files: Seq[File]) {
    def badMode = files.map(getFileExtension).collect { case Some("bad") | Some("mid") => true }.length > 0

    def badFilePrefixOpt: Option[String] = if (!badMode) Some(files.map(_.getName).mkString("_")) else None
  }


  val parser = new scopt.OptionParser[Config]("exam") {
    head("Fast-cards", "3.x")
    arg[Seq[File]]("<file>...") unbounded() action { (x, c) =>

      val partition: (Seq[File], Seq[File]) = x.partition(_.isDirectory)

      val xs = (partition._1.map(d => d.listFiles().filter(_.isFile).toList).flatten ++: partition._2).distinct

      c.copy(files = xs)

    } text "optional unbounded args"
    opt[Unit]("en-ru") action { (_, c) =>
      c.copy(enRu = true)
    } text "en-ru mode"
    opt[Unit]("no-shuffle") action { (_, c) =>
      c.copy(noShuffle = true)
    }
    opt[Int]('p', "pass-count") action { (v, c) =>
      c.copy(passCount = v)
    }

    opt[Seq[String]]('f', "filter") valueName "<type1>,<type2>..." action { (x, c) =>
      c.copy(filter = x)
    } text "filter cards by type (verb, noun, etc)"
  }

  def getFileExtension(file: File): Option[String] = file.getName.split( """\.""").lastOption

  def parseArgs(args: Array[String]): Option[Config] = parser.parse(args, Config())

  def readCards(config: Config): Seq[(File, List[Card])] =
    config.files.map { file =>
      Try {
        val xs: List[Card] = CardsReader.read(file)
        val cards: List[Card] = if (config.filter.nonEmpty) {
          val filter: Seq[Some[String]] = config.filter.map(Some(_))
          xs.filter(c => filter.contains(c.data.kind))
        } else xs
        if (cards.nonEmpty) Some(file, cards) else None
      }.getOrElse(None)
    }.flatten

}
