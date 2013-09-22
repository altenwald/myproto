%% -*- erlang; utf-8 -*-
-module(delete_test).
-author('bombadil@bosqueviejo.net').

-compile(export_all).

% required for eunit to work
-include_lib("eunit/include/eunit.hrl").

-include("sql.hrl").

%%====================================================================
%% Test cases
%%====================================================================

delete_simple_test() ->
    ?assertEqual(mysql_proto:parse("delete from mitabla"),
        #delete{table=#table{name = <<"mitabla">>, alias = <<"mitabla">>}}
    ),
    ok.

delete_where_test() ->
    ?assertEqual(mysql_proto:parse("delete from mitabla where dato='this ain''t a love song'"),
        #delete{
            table=#table{name = <<"mitabla">>, alias = <<"mitabla">>},
            conditions=#condition{
                nexo=eq,
                op1=#key{name = <<"dato">>, alias = <<"dato">>},
                op2=#value{value = <<"this ain't a love song">>}
            }
        }
    ),
    ok.

