-module(my_dummy_handler).
-behaviour(gen_myproto).

-include("../include/myproto.hrl").

-export([check_pass/1, execute/2, metadata/2, terminate/2]).

check_pass(#user{name = User, server_hash = Hash, password = Password}) ->
    case my_request:check_clean_pass(User, Hash) of
        Password -> {ok, Password, []};
        _ -> {error, <<"Password incorrect!">>}
    end.


metadata(version, State) ->
  {reply, <<"dummy handler 1.0">>, State};

metadata({connect_db, _Database}, State) ->
  {noreply, State};

metadata(databases, State) ->
  {reply, [<<"myfakedatabase">>], State};

metadata(_, State) ->
  {noreply, State}.

execute(#request{info = #select{params=[#variable{name = <<"version_comment">>}]}}, State) ->
    Info = {
        [#column{name = <<"@@version_comment">>, type=?TYPE_VARCHAR, length=20}],
        [[<<"myproto 0.1">>]]
    },
    {reply, #response{status=?STATUS_OK, info=Info}, State};
execute(#request{command = ?COM_QUIT}, State) ->
    lager:info("Exiting~n", []),
    {stop, normal, State};
execute(#request{command = ?COM_INIT_DB, info=Database}, State) ->
    lager:info("Change database to: ~p~n", [Database]),
    {reply, #response {
        status=?STATUS_OK, info = <<"Changed to ", Database/binary>>,
        affected_rows = 0, last_insert_id = 0,
        status_flags = 0, warnings = 0
    }, State};
execute(#request{command=?COM_FIELD_LIST, id=_Id, info=_Table}, State) ->
    {reply, #response{
        status=?STATUS_ERR, error_code=2003, %% TODO: found the correct error code
        info = <<"Not implemented field list">>
    }, State};    
execute(#request{command = ?COM_QUERY, info={use, Database}}, State) ->
    lager:info("Change database to: ~p~n", [Database]),
    {reply, #response {
        status=?STATUS_OK, info = <<"Changed to ", Database/binary>>,
        affected_rows = 0, last_insert_id = 0,
        status_flags = 0, warnings = 0
    }, State};
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
