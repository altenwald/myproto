#!/usr/bin/env ERL_LIBS=deps escript

-mode(compile).

main([URL]) ->
  code:add_pathz("ebin"),
  {ok, Sock} = nanomysql:connect(URL),
  loop(Sock).


loop(Sock) ->
  case io:get_line("mysql> ") of
    "exit\n" -> halt(0);
    "quit\n" -> halt(0);
    Query -> {ok, Reply} = nanomysql:execute(Query, Sock),
            print_reply(Reply),
          loop(Sock)
  end.


print_reply({Columns, Rows}) ->
  io:format("~s\n---\n", [ string:join([io_lib:format("~s",[C]) || C <- Columns], ",")]),
  [io:format("~s\n", [ string:join([io_lib:format("~s",[C]) || C <- Row],",")]) || Row <- Rows],
  io:format("\nok\n").


