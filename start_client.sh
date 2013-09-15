#!/bin/sh

erl -pa apps/cli/ebin deps/*/ebin -s lager -s client start
