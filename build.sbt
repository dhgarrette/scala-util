name := "scala-util"

version := "1.0.0"

organization := "dhg"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.10" % "test",
  "com.novocode" % "junit-interface" % "0.6" % "test->default") //switch to ScalaTest at some point...

scalacOptions ++= Seq("-deprecation")
