#!/usr/bin/env ERL_LIBS=deps escript


-mode(compile).

main([]) ->
  code:add_pathz("ebin"),
  lager:start(),
  ok = application:start(myproto),
  receive
    a -> ok
  end.

