#!/bin/bash

projectname=scala-util
version_len=5

rm -rf target/scala-2.12/${projectname}_2.12-*
sbt publish

rm -rf target/scala-2.12/api
sbt doc


for f in target/scala-2.12/${projectname}_2.12-*
do
  len=24+${#projectname}
  version=${f:$len:$version_len}
  if [ -z ${dir+x} ]; then
    dir=$HOME/workspace/maven-repository/snapshots/dhg/${projectname}_2.12/$version-SNAPSHOT/
    echo "Creating directory $dir"
    rm -rf $dir
    mkdir -p $dir
  fi

  cp -r $f $dir
  
  rm -rf ~/.ivy2/cache/dhg/${projectname}_2.12/*-$version*
  rm -rf ~/.ivy2/cache/dhg/${projectname}_2.12/*/*-$version*

done

cp -r target/scala-2.12/api $dir
