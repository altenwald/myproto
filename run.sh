#!/bin/sh


ERL_LIBS=deps erl -sname mysql -pa ebin -s lager -s myproto

