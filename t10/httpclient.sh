#!/usr/bin/env bash

t=$1
x=$2

python2 -m SimpleHTTPServer 8000 &> /dev/null &
sleep .5

./$x

kill -KILL %1
