name := "scala-util"

version := "0.0.2-SNAPSHOT"

organization := "dhg"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.scalaz" %% "scalaz-core" % "7.2.9",
  //"org.spire-math" %% "spire" % "0.8.2",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  //"org.slf4j" % "slf4j-log4j12" % "1.7.9",
  "ch.qos.logback" % "logback-classic" % "1.1.7",
  "junit" % "junit" % "4.12",
  "com.novocode" % "junit-interface" % "0.11" % "test") //switch to ScalaTest at some point...

libraryDependencies ++= Seq(
  // For drawing charts
  "org.jfree" % "jfreechart" % "1.0.19")

libraryDependencies ++= Seq(
  // For drawing trees
  "org.abego.treelayout" % "org.abego.treelayout.netbeans" % "1.0.1" exclude("org.netbeans.api", "org-netbeans-api-visual"),
  "org.codeartisans.thirdparties.swing" % "org-netbeans-api-visual" % "2.23.1")

scalacOptions ++= Seq("-deprecation")

initialCommands in console := "import dhg.util._, scalaz._, Scalaz._"
