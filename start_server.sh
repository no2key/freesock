#!/bin/sh

erl -pa apps/srv/ebin deps/*/ebin -s lager -s server start
