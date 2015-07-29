#!/usr/bin/env ERL_LIBS=deps escript

-mode(compile).

main([URL|_Args]) ->
  code:add_pathz("ebin"),
  {ok, Sock} = nanomysql:connect(URL),
  {ok, {mysql, _, _Host, _Port, "/"++_DBName, _}} = http_uri:parse(URL, [{scheme_defaults,[{mysql,3306}]}]),
  % DB = list_to_binary(DBName),
  % nanomysql:execute("show databases", Sock),
  % {ok, {_, Rows}} = nanomysql:execute("show tables", Sock),
  % [nanomysql:command(4, <<Name/binary,0>>, Sock) || [Name] <- Rows],
  loop(Sock);

main([]) ->
  io:format("~s mysql://user:password@host:port/dbname\n", [escript:script_name()]),
  erlang:halt(2).


loop(Sock) ->
  case io:get_line("mysql> ") of
    "\\?\n" -> help();
    "\\d "++Name1 ->
      Name = string:substr(Name1,1, length(Name1) -1),
      {ok, Reply} = nanomysql:command(show_fields, iolist_to_binary([Name,0]), Sock),
      print_reply(Reply);
    "exit\n" -> halt(0);
    "quit\n" -> halt(0);
    Query -> 
      {ok, Reply} = nanomysql:execute(Query, Sock),
      print_reply(Reply)
  end,
  loop(Sock).


help() ->
  io:format(
"Informational:\n"
% "  \\d      list tables\n"
"  \\d NAME describe table\n"
% "  \\l      list databases\n"
).

print_reply(#{affected_rows := _} = Info) ->
  io:format("info ~p\n", [Info]);

print_reply({Columns}) ->
  print_columns(Columns),
  io:format("\nok\n");

print_reply({Columns, Rows}) ->
  print_columns(Columns),
  print_rows(Rows),
  io:format("\nok\n").

print_columns(Columns) ->
  io:format("~s\n---\n", [ string:join([io_lib:format("~s(~p)",[C,T]) || {C,T} <- Columns], ",")]).

print_rows(Rows) ->
  [io:format("~s\n", [ string:join([io_lib:format("~p",[C]) || C <- Row],",")]) || Row <- Rows].

