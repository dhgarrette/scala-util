sbt publish
scp target/scala-2.10/scala-util_2.10-1.0.0-SNAPSHOT* k:public_html/maven-repository/snapshots/dhg/scala-util_2.10/1.0.0-SNAPSHOT

sbt doc
scp -r target/scala-2.10/api k:public_html/maven-repository/snapshots/dhg/scala-util_2.10/1.0.0-SNAPSHOT
