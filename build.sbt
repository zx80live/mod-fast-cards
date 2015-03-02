import AssemblyKeys._

assemblySettings

jarName in assembly := "mod-fast-cards.jar"

mainClass in assembly := Some("com.zx80live.mod.fastcards.Exam")

excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
  cp filter {
    _.data.getName == "jansi-1.4.jar"
  }
}

name := """mod-fast-cards"""

version := "1.0"

scalaVersion := "2.11.5"

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.9"
libraryDependencies += "org.scala-lang" % "jline" % "2.11.0-M3"
//libraryDependencies += "jline" % "jline" % "2.12.1"
libraryDependencies +=
  ("org.scala-lang" % "jline" % "2.11.0-M3").exclude("org.fusesource.jansi", "jansi")
