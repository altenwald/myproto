%% -*- erlang; utf-8 -*-
-module(update_test).
-author('bombadil@bosqueviejo.net').

-compile(export_all).

% required for eunit to work
-include_lib("eunit/include/eunit.hrl").

-include("sql.hrl").

%%====================================================================
%% Test cases
%%====================================================================

update_simple_test() ->
    ?assertEqual(
        mysql:parse("update mitabla set dato=1"),
        #update{
            table=#table{alias = <<"mitabla">>, name = <<"mitabla">>},
            set=[#set{key = <<"dato">>, value=#value{value=1}}]
        }
    ),
    ?assertEqual(
        mysql:parse(" Update   mitabla SET dato  =  1    "),
        mysql:parse("UPDATE mitabla SET dato=1")
    ),
    ok.

update_multiparams_test() ->
    ?assertEqual(
        mysql:parse("update mitabla set dato1=1, dato2='bon jovi', dato3='this ain''t a love song'"),
        #update{
            table=#table{alias = <<"mitabla">>, name = <<"mitabla">>},
            set=[
                #set{key = <<"dato1">>, value=#value{value = 1}},
                #set{key = <<"dato2">>, value=#value{value = <<"bon jovi">>}},
                #set{key = <<"dato3">>, value=#value{value = <<"this ain't a love song">>}}
            ]
        }
    ),
    ok.

update_where_test() ->
    ?assertEqual(
        mysql:parse("update mitabla set dato=1 where dato=5"),
        #update{
            table=#table{alias = <<"mitabla">>, name = <<"mitabla">>},
            set=[#set{key = <<"dato">>, value=#value{value=1}}],
            conditions=#condition{
                nexo=eq, 
                op1=#key{alias = <<"dato">>, name = <<"dato">>},
                op2=#value{value=5}
            }
        }
    ),
    ok.

