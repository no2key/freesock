#!/bin/bash

MAX_PROCESS=250000
NAME='freesock@127.0.0.1'
COOKIE='cookiefortest'
DEPS='deps/*/ebin'
CONFIG='rel/files/sys'
ERL_OPTS=" -smp auto +P $MAX_PROCESS +A 16 +K true +Q 99999 "

erl $ERL_OPTS -name $NAME -setcookie $COOKIE -config $CONFIG -pa ebin -pa $DEPS \
    -s lager \
    -eval "application:start(ranch)." \
    -eval "application:start(freesock)."
