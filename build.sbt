name := "scala-util"

version := "1.0.0-SNAPSHOT"

organization := "dhg"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "org.scalaz" % "scalaz-core_2.10" % "7.0.3",
  "org.jfree" % "jfreechart" % "1.0.14",
  "com.typesafe" % "scalalogging-log4j_2.10" % "1.0.1",
  "org.apache.logging.log4j" % "log4j-core" % "2.0-beta3",
  "junit" % "junit" % "4.10",
  "com.novocode" % "junit-interface" % "0.8" % "test->default") //switch to ScalaTest at some point...

scalacOptions ++= Seq("-deprecation", "-optimize")

initialCommands in console := "import dhg.util.Arm._, dhg.util.Collections._, dhg.util.CollectionUtil._, dhg.util.FileUtil._, dhg.util.NumberUtil._, dhg.util.Pattern._, dhg.util.StringUtil._, dhg.util.Subprocess, dhg.util.Time._, dhg.util.math.LogDouble, dhg.util.math.NumUtil._, scalaz._, Scalaz._"
