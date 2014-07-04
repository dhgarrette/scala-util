scala-util
==========

Put the following in `build.sbt`:

    resolvers ++= Seq(
      "dhg releases repo" at "http://www.cs.utexas.edu/~dhg/maven-repository/releases",
      "dhg snapshot repo" at "http://www.cs.utexas.edu/~dhg/maven-repository/snapshots"
    )

    libraryDependencies += "dhg" % "scala-util_2.11" % "1.0.0-SNAPSHOT"
    
API available here: [http://www.cs.utexas.edu/~dhg/maven-repository/snapshots/dhg/scala-util_2.11/1.0.0-SNAPSHOT/api]
