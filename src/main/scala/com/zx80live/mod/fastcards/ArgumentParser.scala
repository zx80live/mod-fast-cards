package com.zx80live.mod.fastcards

import com.zx80live.mod.fastcards.util.CardsReader

import scala.util.{Success, Try}

trait ArgumentParser {

  import java.io.File


  case class Config(files: Seq[File] = Seq(), enRu: Boolean = false, filter: Seq[String] = Seq(), noShuffle: Boolean = false, passCount: Int = 2)

  implicit class ConfigExtensions(c: Config) {
    def badMode = c.files.map(getFileExtension).collect { case Some("bad") | Some("mid") => true }.length > 0

    def badFilePrefixOpt: Option[String] = if (!badMode) Some(c.files.map(_.getName).mkString("_")) else None
  }


  val parser = new scopt.OptionParser[Config]("exam") {
    head("Fast-cards", "3.x")
    arg[Seq[File]]("<file>...") unbounded() action { (x, c) =>
      c.copy(files = c.files.++:(x))
    } text "optional unbounded args"
    opt[Unit]("en-ru") action { (_, c) =>
      c.copy(enRu = true)
    } text "en-ru mode"
    opt[Unit]("no-shuffle") action { (_, c) =>
      c.copy(noShuffle = true)
    }
    opt[Int]("pass-count") action { (v, c) =>
      c.copy(passCount = v)
    }

    opt[Seq[String]]('f', "filter") valueName "<type1>,<type2>..." action { (x, c) =>
      c.copy(filter = x)
    } text "filter cards by type (verb, noun, etc)"
  }

  def getFileExtension(file: File): Option[String] = file.getName.split( """\.""").lastOption

  def parseArgs(args: Array[String]): Option[Config] = parser.parse(args, Config())

  def readCards(config: Config): Try[List[Card]] = {
    Try {
      val cards: List[Card] = config.files.map(CardsReader.read).flatten.toList
      val filtered = if (config.filter.nonEmpty) {
        val filter: Seq[Some[String]] = config.filter.map(Some(_))
        cards.filter(c => filter.contains(c.data.kind))
      } else cards

      filtered
    }
  }

}
