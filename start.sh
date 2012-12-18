#!/bin/sh

erl -pa ebin deps/*/ebin -s lager -s piper
