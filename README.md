scala-util
==========

Put the following in `build.sbt`:

    resolvers ++= Seq(
      "dhg releases repo" at "http://www.cs.utexas.edu/~dhg/maven-repository/releases",
      "dhg snapshot repo" at "http://www.cs.utexas.edu/~dhg/maven-repository/snapshots"
    )

    libraryDependencies += "dhg" % "scala-util_2.11" % "0.0.2-SNAPSHOT"
    
API available here: http://www.cs.utexas.edu/~dhg/maven-repository/snapshots/dhg/scala-util_2.11/0.0.2-SNAPSHOT/api

Use the library by simply importing everything in the package:

    import dhg.util._
