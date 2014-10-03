name := "scala-util"

version := "1.0.0-SNAPSHOT"

organization := "dhg"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-math3" % "3.2",
  "org.scalaz" %% "scalaz-core" % "7.0.6", // "7.1.0"
  //"org.spire-math" %% "spire" % "0.8.2",
  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.0",
  "org.slf4j" % "slf4j-log4j12" % "1.7.7",
  "junit" % "junit" % "4.11",
  "com.novocode" % "junit-interface" % "0.10" % "test") //switch to ScalaTest at some point...

libraryDependencies ++= Seq(
  // For drawing charts
  "org.jfree" % "jfreechart" % "1.0.17")

libraryDependencies ++= Seq(
  // For drawing trees
  "org.abego.treelayout" % "org.abego.treelayout.netbeans" % "1.0.1" exclude("org.netbeans.api", "org-netbeans-api-visual"),
  "org.codeartisans.thirdparties.swing" % "org-netbeans-api-visual" % "2.23.1")

scalacOptions ++= Seq("-deprecation")

initialCommands in console := "import dhg.util.Arm._, dhg.util.Collections._, dhg.util.CollectionUtil._, dhg.util.FileUtil._, dhg.util.NumberUtil._, dhg.util.Pattern._, dhg.util.StringUtil._, dhg.util.Subprocess, dhg.util.Time._, dhg.util.math.LogDouble, dhg.util.math.NumUtil._, scalaz._, Scalaz._"
