#!/usr/bin/env bash

t=$1
x=$2

ret=0

./$x &
sleep .1

cmp <(echo -n "test") <(curl -s http://localhost:8000/ --data "test") \
	|| { ret=$?; echo "failed cmp"; }

# When the request body is over 1024 in length, curl first checks with the
# server and expects a 100-Continue.
test $(curl -s http://localhost:8000/ --data-binary @<(head -c1025 /dev/zero) |wc -c) == 1025 \
	|| { ret=$?; echo "failed 100-Continue"; }

kill -KILL %1

exit $ret
