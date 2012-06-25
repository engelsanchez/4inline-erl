#!/bin/sh
erl -sname c4_server -pa ebin -pa deps/*/ebin -s c4_server \
	-eval "io:format(\"Server listening on http://localhost:8080/websocket~n\")." 
