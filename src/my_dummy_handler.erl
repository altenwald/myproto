-module(my_dummy_handler).
-behaviour(gen_myproto).

-include("../include/myproto.hrl").

-export([check_pass/3, execute/2, terminate/2]).

check_pass(User, Hash, Password) ->
    case my_request:check_clean_pass(User, Hash) of
        Password -> {ok, Password, []};
        _ -> {error, <<"Password incorrect!">>}
    end.

execute(#request{info = #select{params=[#variable{name = <<"version_comment">>}]}}, State) ->
    Info = {
        [#column{name = <<"@@version_comment">>, type=?TYPE_VARCHAR, length=20}],
        [[<<"myproto 0.1">>]]
    },
    {reply, #response{status=?STATUS_OK, info=Info}, State};
execute(#request{command = ?COM_QUIT}, State) ->
    lager:info("Exiting~n", []),
    {stop, normal, State};
execute(Request, State) ->
    lager:info("Request: ~p~n", [Request]),
    Info = {
        [
            #column{name = <<"Info">>, type=?TYPE_VARCHAR, length=20},
            #column{name = <<"Info2">>, type=?TYPE_VARCHAR, length=20}
        ],
        [
            [<<"Not implemented!">>, <<"Yet">>],
            [<<"Testing MultiColumn!">>, <<"Still">>]
        ]
    },
    {reply, #response{status=?STATUS_OK, info=Info}, State}.

terminate(_Reason, _State) ->
    ok.
