name := "cl-scala-games"

version := "0.01"

organization := "org.christopherglucas"

scalaVersion := "2.11.2"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.11.0-M7"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.2"

libraryDependencies += "org.christopherglucas" % "scala-sandbox_2.11" % "0.12"

scalacOptions += "-deprecation"

scalacOptions += "-feature"
