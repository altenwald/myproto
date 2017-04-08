-module(test_handler).
-author('Manuel Rubio <manuel@altenwald.com>').

-behaviour(gen_myproto).

-include_lib("myproto/include/myproto.hrl").

-export([
    start_server/0,
    stop_server/1,
    table_columns/1,
    tables/0,
    check_pass/1,
    execute/2,
    terminate/2,
    metadata/2
]).

-define(ERR_WRONG_PASS, {error, <<"Password incorrect!">>}).
-define(ERR_WRONG_USER, {error, <<"No such user!">>}).
-define(ERR_LOGIN_DISABLED, {error, <<"Login disabled">>}).
-define(ERR_INFO(Code, Desc),
        #response{status = ?STATUS_ERR, error_code = Code, info = Desc}).

-record(my, {
    db
}).


start_server() ->
    Opts = [binary, {reuseaddr, true}, {active, false}],
    {ok, LSock} = gen_tcp:listen(0, Opts),
    {ok, Port} = inet:port(LSock),
    ok = gen_tcp:close(LSock),
    myproto:start_link(Port, ?MODULE, true).


stop_server(PID) ->
    myproto:stop(PID).


check_pass(#user{name = <<"user">>, password = Pass} = _User) ->
    {ok, Pass, #my{}};

check_pass(_) ->
    ?ERR_WRONG_PASS.


metadata(version, State) ->
    {reply, <<"5.6.0">>, State};

metadata({connect_db, <<"test_db">>}, State) ->
    {noreply, State};

metadata(databases, State) ->
    {reply, [<<"test_db">>], State};

metadata(tables, State) ->
    {reply, {<<"test_db">>, tables()}, State};

metadata({fields, Table}, State) ->
    {reply, {<<"test_db">>, Table, table_columns(Table)}, State};

metadata(_, State) ->
    {noreply, State}.


tables() ->
    <<"test">>.
table_columns(<<"test">>) ->
    [{id, string},
     {name, string},
     {url, string}].


execute(#request{info = #select{tables = [#table{name = Table}]} = _Select},
        #my{} = State) ->
    TableColumns = table_columns(Table),
    Columns = [ N || {N,_} <- TableColumns ],
    Rows = [ [{id, <<"1">>}, {name, <<"stream1">>}, {url, <<"rtsp://...">>}] ],
    ResponseColumns = [
        case lists:keyfind(Name, 1, table_columns(Table)) of
            false -> {Name, string};
            Col -> Col
        end || Name <- Columns ],
    Response = { response_columns(ResponseColumns),
                 [response_row(Row, Columns) || Row <- Rows] },
    {reply, #response{status=?STATUS_OK, info = Response}, State};

execute(#request{} = _Request, #my{} = State) ->
    {reply, default, State}.


response_row(Row, Columns) when is_list(Row) ->
    [ proplists:get_value(Column, Row) || Column <- Columns ].


to_col({Name, string}) ->
    #column{name = atom_to_binary(Name, latin1),
            type = ?TYPE_VAR_STRING,
            length = 20,
            org_name = atom_to_binary(Name,latin1)};

to_col({Name, boolean}) ->
    #column{name = atom_to_binary(Name, latin1),
            type = ?TYPE_TINY,
            length = 1,
            org_name = atom_to_binary(Name, latin1)};

to_col({Name, integer}) ->
    #column{name = atom_to_binary(Name, latin1),
            type = ?TYPE_LONGLONG,
            length = 20,
            org_name = atom_to_binary(Name, latin1)}.


response_columns(Columns) ->
    lists:map(fun to_col/1, Columns).


terminate(_Reason, _State) ->
    ok.
