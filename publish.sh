#!/bin/bash

projectname=scala-util
version_len=5

rm -rf target/scala-2.11/${projectname}_2.11-*
sbt publish

rm -rf target/scala-2.11/api
sbt doc


for f in target/scala-2.11/${projectname}_2.11-*
do
  len=24+${#projectname}
  version=${f:$len:$version_len}
  dir=public_html/maven-repository/snapshots/dhg/${projectname}_2.11/$version-SNAPSHOT/
  ssh k mkdir -p $dir
  scp $f k:$dir
  
  rm -rf ~/.ivy2/cache/dhg/${projectname}_2.11/*-$version*
  rm -rf ~/.ivy2/cache/dhg/${projectname}_2.11/*/*-$version*

done

scp -r target/scala-2.11/api k:public_html/maven-repository/snapshots/dhg/${projectname}_2.11/$version-SNAPSHOT

