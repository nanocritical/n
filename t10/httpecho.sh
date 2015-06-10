#!/usr/bin/env bash

t=$1
x=$2

ret=0

./$x &> /dev/null &
sleep .1

cmp <(echo -n "test") <(curl -s http://localhost:8000/ --data "test") \
	|| { ret=$?; echo "failed cmp"; }

kill -KILL %1

exit $ret
