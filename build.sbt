name := "scala-util"

version := "1.0.0"

organization := "dhg"

scalaVersion := "2.10.0"

crossPaths := false

libraryDependencies ++= Seq(
  "commons-logging" % "commons-logging" % "1.1.1",
  "log4j" % "log4j" % "1.2.16",
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.6" % "test->default") //switch to ScalaTest at some point...

