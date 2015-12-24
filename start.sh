#!/bin/sh
erl -pa ebin -pa deps/*/ebin -eval "chrip_server_app:start(0, 0)."
