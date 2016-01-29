#!/bin/bash

name=lifelines

sbt compile || { RET="$?"; notify sbt "Compile $name failed" ; exit "$RET" ; }

./watch.sh &
watch_pid="$?"

sbt -Dconfig.file=conf/application-local.conf run

kill "$watch_pid" &> /dev/null
sleep 1 &> /dev/null
kill -9 "$watch_pid" &> /dev/null
