package com.zx80live.mod.fastcards

import java.io.{FilenameFilter, File}

import com.zx80live.mod.fastcards.util.CardsReader

import scala.util.{Failure, Success, Try}

trait ArgumentParser {

  import java.io.File


  case class Config(files: Seq[File] = Seq(),
                    enRu: Boolean = false,
                    filter: Seq[String] = Seq(),
                    noShuffle: Boolean = false,
                    passCount: Int = 2,
                    randomWords: Option[Int] = None,
                    noMakeBads: Boolean = false,
                    verboseParse: Boolean = false)

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
    head("Fast-cards", "1.0")
    arg[Seq[File]]("<file>...") unbounded() action { (x, c) =>

      // prepare *.bad
      val midBadPartition: (Seq[File], Seq[File]) = x.partition(f => List("*.bad", "*.mid").contains(f.getName))

      val midBadFiles: Seq[File] = midBadPartition._1.map { f =>
        new File(".").listFiles(new FilenameFilter() {
          override def accept(dir: File, name: String): Boolean = name.endsWith(f.getName.takeRight(3))
        })
      }.flatten


      val partition: (Seq[File], Seq[File]) = midBadPartition._2.partition(_.isDirectory)

      val xs = partition._1.map(d => d.listFiles().filter(_.isFile).toList).flatten ++: partition._2

      val resultFiles: Seq[File] = (c.files ++: xs ++: midBadFiles).map(_.getCanonicalFile).distinct

      c.copy(files = resultFiles)

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
    opt[Int]('r', "random-words") action { (v, c) =>
      c.copy(randomWords = Some(v))
    }
    opt[Unit]("no-make-bads") action { (v, c) =>
      c.copy(noMakeBads = true)
    }
    opt[Unit]("verbose-parse") action { (v, c) =>
      c.copy(verboseParse = true)
    }
    opt[Seq[String]]('f', "filter") valueName "<type1>,<type2>..." action { (x, c) =>
      c.copy(filter = x)
    } text "filter cards by type (verb, noun, etc)"
  }

  def getFileExtension(file: File): Option[String] = file.getName.split( """\.""").lastOption

  def parseArgs(args: Array[String]): Option[Config] = parser.parse(args, Config())


  def readCards(config: Config): Seq[(File, Try[List[Card]])] = {
    config.files.map { file =>
      val triedCards: Try[List[Card]] = Try {
        val xs: List[Card] = CardsReader.read(file)
        if (config.filter.nonEmpty) {
          val filter: Seq[Some[String]] = config.filter.map(Some(_))
          xs.filter(c => filter.contains(c.data.kind))
        } else xs
      } match {
        case Success(Nil) => Failure(new CardsNotFoundException(file))
        case r@_ => r
      }
      (file, triedCards)
    }
  }
}

class CardsNotFoundException(file: File) extends IllegalArgumentException