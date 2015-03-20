import AssemblyKeys._

assemblySettings

jarName in assembly := "mod-fast-cards.jar"

mainClass in assembly := Some("com.zx80live.mod.fastcards.ExamController")

excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
  cp filter {
    _.data.getName == "jansi-1.4.jar"
  }
}

name := """fast-cards"""

organization := "com.zx80.mod"

version := "1.0"

scalaVersion := "2.11.6"

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies += "com.zx80.util" %% "console-utils" % "1.0"
libraryDependencies += "com.jsuereth" % "scala-arm_2.11" % "1.4"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.9",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.9" % "test")



libraryDependencies += "org.scala-lang" % "jline" % "2.11.0-M3"

libraryDependencies +=
  ("org.scala-lang" % "jline" % "2.11.0-M3").exclude("org.fusesource.jansi", "jansi")
