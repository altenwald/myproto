%% -*- erlang; utf-8 -*-
-module(insert_test).
-author('bombadil@bosqueviejo.net').

-compile(export_all).

% required for eunit to work
-include_lib("eunit/include/eunit.hrl").

-include("sql.hrl").

%%====================================================================
%% Test cases
%%====================================================================

insert_simple_test() ->
    ?assertEqual(
        #insert{table = #table{name = <<"mitabla">>, alias = <<"mitabla">>}, values=[
            #value{value=1}, #value{value=2}, #value{value=3}
        ]},
        mysql_proto:parse("insert into mitabla values (1,2,3)")
    ),
    ok.

insert_keys_test() ->
    ?assertEqual(
        #insert{table = #table{name = <<"mitabla">>,
                       alias = <<"mitabla">>},
        values = [#set{key = <<"id">>,
                       value = #value{value = 1}},
                  #set{key = <<"author">>,
                       value = #value{value = <<"bonjovi">>}},
                  #set{key = <<"song">>,
                       value = #value{value = <<"these days">>}}]},
        mysql_proto:parse("insert into mitabla(id,author,song) values(1,'bonjovi', 'these days')")
    ),
    ok.

insert_set_test() ->
    ?assertEqual(
        mysql_proto:parse("insert into mitabla(id,author,song) values(1,'bonjovi', 'these days')"),
        mysql_proto:parse("insert into mitabla set id=1, author='bonjovi', song='these days'")
    ),
    ok.

