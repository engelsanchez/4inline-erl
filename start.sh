#!/bin/sh
erl -sname c4_server -pa ebin -pa deps/*/ebin -s c4_server 
