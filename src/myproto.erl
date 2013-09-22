-module(myproto).
-export([start/0]).

start() ->
  application:start(myproto).

