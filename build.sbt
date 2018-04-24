name := "scala-util"

version := "0.0.4-SNAPSHOT"

organization := "dhg"

scalaVersion := "2.12.5"

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.scalaz" %% "scalaz-core" % "7.2.20",
  //"org.typelevel" %% "spire" % "0.15.0",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.8.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
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
