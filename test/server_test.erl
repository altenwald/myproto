%% -*- erlang; utf-8 -*-
-module(server_test).
-author('Max Lapshin <max@maxidoors.ru>').

-compile(export_all).

% required for eunit to work
-include_lib("eunit/include/eunit.hrl").
-include("myproto.hrl").


%%====================================================================
%% Test cases
%%====================================================================


select_simple_test() ->
  {ok, LSocket} = gen_tcp:listen(0, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
  {ok, ListenPort} = inet:port(LSocket),
  Client = spawn_link(fun() ->
    {ok, Sock} = nanomysql:connect("mysql://user:pass@127.0.0.1:"++integer_to_list(ListenPort)++"/dbname"),
    ok
  end),
  {ok, Sock} = gen_tcp:accept(LSocket),
  {ok, Hello, My1} = my_protocol:hello(42),
  ok = gen_tcp:send(Sock, Hello),
  % inet:setopts(Sock, [{active,true}]),
  {ok, Bin} = gen_tcp:recv(Sock, 0),
  {ok, #request{info = #user{}}, My2} = my_protocol:decode(Bin, My1),
  {ok, Ok1, My3} = my_protocol:ok(My2),
  ok = gen_tcp:send(Sock, Ok1),
  ok.

