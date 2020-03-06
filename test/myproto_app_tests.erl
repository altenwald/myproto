-module(myproto_app_tests).
-author('Manuel Rubio <manuel@altenwald.com>').
-compile([warnings_as_errors]).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_VERSION, <<"5.6.0-test">>).

mysql_start(Port, User, DB) ->
    mysql:start_link([{host, "localhost"},
                      {port, Port},
                      {user, User},
                      {password, <<"user">>},
                      {database, DB}]).

start_conn() ->
    ok = application:load(myproto),
    ok = application:set_env(myproto, handler, test_handler),
    ok = application:set_env(myproto, port, 0),
    ok = application:set_env(myproto, server_sign, ?TEST_VERSION),
    ok = application:start(sqlparser),
    ok = application:start(myproto),
    {ok, Port} = gen_server:call(my_acceptor, port),
    Port.

stop_conn(_Port) ->
    ok = application:stop(myproto),
    ok = application:stop(sqlparser),
    ok = application:unload(myproto),
    ok.

connection_test_() ->
    {foreach,
        fun start_conn/0,
        fun stop_conn/1,
        [
            fun connection_no_such_user/1,
            fun connection_wrong_pass/1,
            fun connection_disabled_login/1,
            fun connection_ok/1
        ]
    }.

connection_no_such_user(Port) ->
    erlang:process_flag(trap_exit, true),
    {error, Ret} = mysql_start(Port, <<"user1">>, <<"dbtest">>),
    ?_assertEqual({1045, <<"42000">>, <<"No such user!">>}, Ret).

connection_wrong_pass(Port) ->
    erlang:process_flag(trap_exit, true),
    {error, Ret} = mysql_start(Port, <<"root">>, <<"dbtest">>),
    ?_assertEqual({1045, <<"42000">>, <<"Password incorrect!">>}, Ret).

connection_disabled_login(Port) ->
    erlang:process_flag(trap_exit, true),
    {error, Ret} = mysql_start(Port, <<"hack">>, <<"dbtest">>),
    ?_assertEqual({1289, <<"28000">>, <<"Login disabled">>}, Ret).

connection_ok(Port) ->
    ?_test(begin
        {ok, _Pid} = mysql_start(Port, <<"user">>, <<"dbtest">>),
        ok
    end).

start_query() ->
    Port = start_conn(),
    {ok, Pid} = mysql_start(Port, <<"user">>, <<"dbtest">>),
    {Pid, Port}.

stop_query({Pid, Port}) ->
    unlink(Pid),
    exit(Pid, shutdown),
    stop_conn(Port).

query_test_() ->
    {foreach,
        fun start_query/0,
        fun stop_query/1,
        [
            fun query_version/1,
            fun query_show_databases/1,
            fun query_show_tables/1,
            fun query_describe/1,
            fun query_select/1,
            fun query_update/1
        ]
    }.

query_version({Pid, _Port}) ->
    {ok, _Columns, Rows} = mysql:query(Pid, <<"SELECT @@version_comment">>),
    ?_assertEqual([[?TEST_VERSION]], Rows).

query_show_databases({Pid, _Port}) ->
    {ok, _Columns, Rows} = mysql:query(Pid, <<"SHOW DATABASES">>),
    ?_assertEqual([[<<"test_db">>]], Rows).

query_show_tables({Pid, _Port}) ->
    {ok, _Columns, Rows} = mysql:query(Pid, <<"SHOW TABLES">>),
    ?_assertEqual([[<<"test">>]], Rows).

query_describe({Pid, _Port}) ->
    {ok, _Columns, Rows} = mysql:query(Pid, <<"DESC test">>),
    ?_assertEqual([
        [<<"id">>,<<"varchar(255)">>,<<"YES">>,<<>>,null,<<>>],
        [<<"name">>,<<"varchar(255)">>,<<"YES">>,<<>>,null,<<>>],
        [<<"url">>,<<"varchar(255)">>,<<"YES">>,<<>>,null,<<>>]
    ], Rows).

query_select({Pid, _Port}) ->
    {ok, _Columns, Rows} = mysql:query(Pid, <<"SELECT * FROM test">>),
    ?_assertEqual([[<<"1">>,<<"stream1">>,<<"rtsp://...">>]], Rows).

query_update({Pid, _Port}) ->
    ?_test(begin
        ok = mysql:query(Pid, <<"UPDATE test SET a = 1">>)
    end).
