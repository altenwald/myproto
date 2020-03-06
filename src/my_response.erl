-module(my_response).
-author('Manuel Rubio <manuel@altenwald.com>').

-include("myproto.hrl").

-define(SQL_PARAM(Name),
        #select{params = [#variable{name = Name}]}).
-define(SQL_PARAM(Name, Scope),
        #select{params = [#variable{name = Name, scope = Scope}]}).

-define(COL(Name, Type, Length),
        #column{name = Name, type = Type, length = Length}).

-export([
    send_or_reply/2,
    ok/1,
    error/2,
    error/3,
    default_reply/3
]).


send_or_reply(ok, Socket) ->
    ok(Socket);

send_or_reply(#response{} = Response, Socket) ->
    error_logger:info_msg("response: ~p~n", [Response]),
    ok = gen_tcp:send(Socket, my_packet:encode(Response)).


ok(Socket) ->
    Response = #response{status = ?STATUS_OK,
                         status_flags = ?SERVER_STATUS_AUTOCOMMIT},
    send_or_reply(Response, Socket).


error(Reason, Socket) when is_binary(Reason) ->
    error(1045, Reason, Socket).

error(Code, Reason, Socket) when is_integer(Code), is_binary(Reason) ->
    Response = #response{status = ?STATUS_ERR,
                         error_code = Code,
                         info = Reason},
    send_or_reply(Response, Socket).


default_reply(#request{info = ?SQL_PARAM(<<"version_comment">>)},
              Handler, State) ->
    {Version, State1} = case Handler:metadata(version, State) of
        {reply, Vsn, State1_} -> {iolist_to_binary(Vsn), State1_};
        {noreply, State1_} -> {<<"erlang myproto server">>, State1_}
    end,
    Info = {
        [?COL(<<"@@version_comment">>, ?TYPE_VARCHAR, 20)],
        [[Version]]
    },
    {reply, #response{status=?STATUS_OK, info=Info}, State1};

default_reply(#request{info = ?SQL_PARAM(<<"global.max_allowed_packet">>)},
              _Handler, State) ->
    Info = {
        [?COL(<<"@@global.max_allowed_packet">>, ?TYPE_LONG, 20)],
        [[4194304]]
    },
    {reply, #response{status=?STATUS_OK, info=Info}, State};

default_reply(#request{info = ?SQL_PARAM(<<"tx_isolation">>, local)},
              _Handler, State) ->
    Info = {
        [?COL(<<"@@tx_isolation">>, ?TYPE_VARCHAR, undefined)],
        [[<<"REPEATABLE-READ">>]]
    },
    {reply, #response{status=?STATUS_OK, info=Info}, State};

default_reply(#request{command = ?COM_INIT_DB, info = Database}, Handler, State) ->
    {noreply, State1} = Handler:metadata({connect_db, Database}, State),
    {reply,
     #response{status = ?STATUS_OK,
               info = <<"Changed to ", Database/binary>>,
               status_flags = 2},
     State1};

default_reply(#request{command = init_db, info = Database}, Handler, State) ->
    {noreply, State1} = Handler:metadata({connect_db, Database}, State),
    {reply,
     #response{status = ?STATUS_OK,
               info = <<"Changed to ", Database/binary>>},
     State1};

default_reply(#request{info = #show{type = databases}}, Handler, State) ->
    {Databases, State1} = case Handler:metadata(databases, State) of
        {reply, DB, State1_} -> {DB, State1_};
        {noreply, State1_} -> {[<<"myproto">>], State1_}
    end,
    ResponseFields = {
        [#column{name = <<"Database">>,
                 type = ?TYPE_VAR_STRING,
                 length = 20,
                 schema = <<"information_schema">>,
                 table = <<"SCHEMATA">>,
                 org_table = <<"SCHEMATA">>,
                 org_name = <<"SCHEMA_NAME">>}],
        [ [DB] || DB <- Databases ]
    },
    Response = #response{status=?STATUS_OK, info = ResponseFields},
    {reply, Response, State1};

default_reply(#request{info = #show{type = collation}}, _Handler, State) ->
    ResponseFields = {
        [#column{name = <<"Collation">>, type = ?TYPE_VAR_STRING, length = 20},
         #column{name = <<"Charset">>, type = ?TYPE_VAR_STRING, length = 20},
         #column{name = <<"Id">>, type = ?TYPE_LONG},
         #column{name = <<"Default">>, type = ?TYPE_VAR_STRING, length = 20},
         #column{name = <<"Compiled">>, type = ?TYPE_VAR_STRING, length = 20},
         #column{name = <<"Sortlen">>, type = ?TYPE_LONG}],
        [[<<"utf8_bin">>, <<"utf8">>, 83, <<>>, <<"Yes">>, 1]]
    },
    {reply, #response{status=?STATUS_OK, info = ResponseFields}, State};

default_reply(#request{info = #show{type = variables}}, Handler, State) ->
    {Version, State1} = case Handler:metadata(version, State) of
        {reply, Vsn, State1_} -> {iolist_to_binary(Vsn), State1_};
        {noreply, State1_} -> {<<"5.6.0">>, State1_}
    end,
    {Mega, Sec, Micro} = os:timestamp(),
    ITS = Mega * 1000000 + Sec,
    Timestamp = iolist_to_binary(io_lib:format("~B.~6..0B", [ITS, Micro])),

    RCol = #column{type = ?TYPE_VAR_STRING,
                   length = 20,
                   schema = <<"information_schema">>,
                   table = <<"SCHEMATA">>,
                   org_table = <<"SCHEMATA">>,
                   org_name = <<"SCHEMA_NAME">>},
    ResponseFields = {
        [RCol#column{name = <<"Variable_name">>},
         RCol#column{name = <<"Value">>}],
        [
            [<<"sql_mode">>, <<"NO_ENGINE_SUBSTITUTION">>],
            [<<"auto_increment_increment">>, <<"1">>],
            [<<"character_set_client">>, <<"utf8">>],
            [<<"character_set_connection">>, <<"utf8">>],
            [<<"character_set_database">>, <<"utf8">>],
            [<<"character_set_results">>, <<"utf8">>],
            [<<"character_set_server">>, <<"utf8">>],
            [<<"character_set_system">>, <<"utf8">>],
            [<<"date_format">>, <<"%Y-%m-%d">>],
            [<<"datetime_format">>, <<"%Y-%m-%d %H:%i:%s">>],
            [<<"default_storage_engine">>, <<"myproto">>],
            [<<"timestamp">>, Timestamp],
            [<<"version">>, Version]
        ]
    },
  {reply, #response{status=?STATUS_OK, info = ResponseFields}, State1};

default_reply(#request{info = ?SQL_PARAM(<<"DATABASE">>)}, _Handler, State) ->
    ResponseFields = {
        [#column{name = <<"DATABASE()">>,
                 type = ?TYPE_VAR_STRING,
                 length = 102,
                 flags = 0,
                 decimals = 31}],
        []
    },
    Response = #response{status=?STATUS_OK, info = ResponseFields},
    {reply, Response, State};

default_reply(#request{info = #show{type = tables}}, Handler, State) ->
    {DB, Tables, State1} = case Handler:metadata(tables, State) of
        {reply, {DB_, Tables_}, State1_} -> {DB_, Tables_, State1_};
        {noreply, State1_} -> {<<"myproto">>, [], State1_}
    end,
    ResponseFields = {
        [#column{name = <<"Tables_in_", DB/binary>>,
                 type = ?TYPE_VAR_STRING,
                 schema = DB,
                 table = <<"TABLE_NAMES">>,
                 org_table = <<"TABLE_NAMES">>,
                 org_name = <<"TABLE_NAME">>,
                 flags = 256,
                 length = 192,
                 decimals = 0}],
        [ [Table] || Table <- Tables ]
    },
    Response = #response{status=?STATUS_OK, info = ResponseFields},
    {reply, Response, State1};

default_reply(#request{info = #describe{table = #table{name = Table}}},
              Handler, State) ->
    Show = #show{type = fields, from = Table, full = false},
    default_reply(#request{info = Show}, Handler, State);

default_reply(#request{info = #show{type = fields, from = Table, full = Full}},
              Handler, State) ->
    {reply, {_DB, Table, Fields}, State1} =
        Handler:metadata({fields, Table}, State),
    RCol = #column{table = <<"COLUMNS">>,
                   org_table = <<"COLUMNS">>,
                   schema = <<"information_schema">>,
                   flags = 256},
    Header = [
        RCol#column{name = <<"Field">>,
                    org_name = <<"COLUMN_NAME">>,
                    type = ?TYPE_VAR_STRING,
                    length = 192},
        RCol#column{name = <<"Type">>,
                    org_name = <<"COLUMN_TYPE">>,
                    type = ?TYPE_BLOB,
                    length = 589815},
        RCol#column{name = <<"Null">>,
                    org_name = <<"IS_NULLABLE">>,
                    type = ?TYPE_VAR_STRING,
                    length = 9},
        RCol#column{name = <<"Key">>,
                    org_name = <<"COLUMN_KEY">>,
                    type = ?TYPE_VAR_STRING,
                    length = 9},
        RCol#column{name = <<"Default">>,
                    org_name = <<"COLUMN_DEFAULT">>,
                    type = ?TYPE_BLOB,
                    length = 589815},
        RCol#column{name = <<"Extra">>,
                    org_name = <<"EXTRA">>,
                    type = ?TYPE_VAR_STRING,
                    length = 90}
    ] ++ case Full of
        true -> [
            RCol#column{name = <<"Collation">>,
                        org_name = <<"COLLATION_NAME">>,
                        type = ?TYPE_VAR_STRING,
                        length = 96},
            RCol#column{name = <<"Privileges">>,
                        org_name = <<"PRIVILEGES">>,
                        type = ?TYPE_VAR_STRING,
                        length = 240},
            RCol#column{name = <<"Comment">>,
                        org_name = <<"COLUMN_COMMENT">>,
                        type = ?TYPE_VAR_STRING,
                        length = 3072}
        ];
        false -> []
    end,
    Rows = lists:map(fun({Name, Type}) ->
        [to_b(Name),
         case Type of
            string -> <<"varchar(255)">>;
            boolean -> <<"tinyint(1)">>;
            _ -> <<"bigint(20)">>
         end,
         <<"YES">>,
         <<>>,
         undefined,
         <<>>
        ] ++ case Full of
            true -> [
                case Type of
                    string -> <<"utf8_general_ci">>;
                    _ -> undefined
                end,
                <<"select,insert,update,references">>,
                <<>>
            ];
            false -> []
        end
    end, Fields),
    {reply, #response{status=?STATUS_OK, info = {Header, Rows}}, State1};

default_reply(#request{info = #show{type = create_table, from = Table}},
              Handler, State) ->
    {reply, {_DB, Table, Fields}, State1} =
        Handler:metadata({fields, Table}, State),
    CreateTable = iolist_to_binary([
        "CREATE TABLE `", Table, "` (\n",
        tl(lists:flatmap(fun({Name,Type}) ->
            [",`",
             to_b(Name),
             "` ",
             case Type of
                string -> "varchar(255)";
                integer -> "bigint(20)";
                boolean -> "tinyint(1)"
             end,
             "\n"]
        end, Fields)),
        ")"
    ]),
    RCol = #column{flags = 256, decimals = 31, type = ?TYPE_VAR_STRING},
    Response = {
        [RCol#column{name = <<"Table">>, length = 192},
         RCol#column{name = <<"Create Table">>, length = 3072}],
        [[Table, CreateTable]]
    },
    {reply, #response{status=?STATUS_OK, info = Response}, State1};

default_reply(#request{command = field_list, info = Table}, Handler, State) ->
    {reply, {DB, Table, Fields}, State1} =
        Handler:metadata({fields, Table}, State),

    Reply = [#column{schema = DB,
                     table = Table,
                     org_table = Table,
                     name = to_b(Field),
                     org_name = to_b(Field),
                     length = 20,
                     type = case Type of
                        string -> ?TYPE_VAR_STRING;
                        integer -> ?TYPE_LONGLONG;
                        boolean -> ?TYPE_TINY
                     end} || {Field, Type} <- Fields ],
    {reply, #response{status = ?STATUS_OK, info = {Reply}}, State1};

default_reply(#request{command = ping}, _Handler, State) ->
    {reply, #response{status = ?STATUS_OK, id = 1}, State};

default_reply(#request{command = ?COM_QUIT}, _Handler, State) ->
    {reply, #response{status = ?STATUS_OK, id = 1}, State}.


to_b(Atom) when is_atom(Atom) -> atom_to_binary(Atom, latin1);
to_b(Bin) when is_binary(Bin) -> Bin.
