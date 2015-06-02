-module(my_ranch_worker).

-export([start_server/4, stop_server/1, start_link/4, init_server/4]).

-export([handle_call/3, handle_info/2, terminate/2]).
-include("../include/myproto.hrl").


start_server(Port, Name, Handler, Args) ->
  application:start(ranch),
  ranch:start_listener(Name, 10, ranch_tcp, [{port, Port},{backlog,4096},{max_connections,32768}], ?MODULE, [Handler, Args]).

stop_server(Name) ->
  ranch:stop_listener(Name).


start_link(ListenerPid, Socket, _Transport, [Handler, Args]) ->
  proc_lib:start_link(?MODULE, init_server, [ListenerPid, Socket, Handler, Args]).



-record(server, {
  handler,
  state,
  socket,
  my
}).

init_server(ListenerPid, Socket, Handler, _Args) ->
  proc_lib:init_ack({ok, self()}),
  ranch:accept_ack(ListenerPid),

  My0 = my_protocol:init([{socket, Socket},{parse_query,true}]),
  {ok, My1} = my_protocol:hello(42, My0),
  case my_protocol:next_packet(My1) of
    {ok, #request{info = #user{password = Password} = User}, My2} ->
      case Handler:check_pass(User) of
        {ok, Password, HandlerState} ->
          {ok, My3} = my_protocol:ok(My2),
          inet:setopts(Socket, [{active,once}]),
          State = #server{handler = Handler, state = HandlerState, socket = Socket, my = My3},
          gen_server:enter_loop(?MODULE, [], State);
        {error, Reason} ->
          my_protocol:error(Reason, My2);
        {error, Code, Reason} ->
          my_protocol:error(Code, Reason, My2)
      end;
    {error, StartError} ->
      {stop, StartError}
  end.


handle_call(Call, _From, #server{} = Server) ->
  {stop, {unknown_call,Call}, Server}.


handle_info({tcp, Socket, Bin}, #server{my = My} = Server) ->
  My1 = my_protocol:buffer_bytes(Bin, My),
  inet:setopts(Socket, [{active, once}]),
  try handle_packets(Server#server{my = My1})
  catch
    Class:Error -> 
      lager:info("~p:~p during handling mysql:\n~p\n", [Class, Error, erlang:get_stacktrace()]),
      {stop, normal, Server}
  end;

handle_info({tcp_closed, _Socket}, #server{handler = Handler, state = HandlerState} = Server) ->
  Handler:terminate(tcp_closed, HandlerState),
  {stop, normal, Server};

handle_info({tcp_error, _, Error}, #server{handler = Handler, state = HandlerState} = Server) ->
  Handler:terminate({tcp_error, Error}, HandlerState),
  {stop, normal, Server}.


terminate(_,_) -> ok.



handle_packets(#server{my = My, handler = Handler, state = HandlerState} = Server) ->
  case my_protocol:decode(My) of
    {ok, Query, My1} ->
      % #request{text = Text, command = Command} = Query,
      % lager:info("~s ~s", [Command, Text]),
      try Handler:execute(Query, HandlerState) of
        {noreply, HandlerState1} ->
          handle_packets(Server#server{my = My1, state = HandlerState1});
        {reply, default, HandlerState1} when Query#request.command == quit ->
          Handler:terminate(quit, HandlerState1),
          {stop, normal, Server#server{my = My1, state = HandlerState1}};
        {reply, default, HandlerState1} ->
          {reply, Reply, HandlerState2} = default_reply(Query, Handler, HandlerState1),
          % lager:info("output ~p", [Reply]),
          {ok, My2} = my_protocol:send_or_reply(Reply, My1),
          handle_packets(Server#server{my = My2, state = HandlerState2});          
        {reply, Response, HandlerState1} ->
          % lager:info("output ~p", [Response]),
          {ok, My2} = my_protocol:send_or_reply(Response, My1),
          handle_packets(Server#server{my = My2, state = HandlerState1});
        {stop, Reason, HandlerState1} ->
          Handler:terminate(Reason, HandlerState1),
          {stop, Reason, Server#server{my = My1, state = HandlerState1}}
      catch
        C:E ->
          ST = erlang:get_stacktrace(),
          lager:info("~p:~p in MySQL server handler\n"
            "  * Last input: ~p\n"
            "  * Stacktrace: ~p\n"
            "  * State: ~p", [C, E, Query, ST, HandlerState]),
          {stop, E, Server}
      end;
    {more, My1} ->
      {noreply, Server#server{my = My1}}
  end.








default_reply(#request{info = #select{params=[#variable{name = <<"version_comment">>}]}}, Handler, State) ->
  {Version, State1} = case Handler:metadata(version, State) of
    {reply, Vsn, State1_} -> {iolist_to_binary(Vsn), State1_};
    {noreply, State1_} -> {<<"erlang myproto server">>, State1_}
  end,
  Info = {
    [#column{name = <<"@@version_comment">>, type=?TYPE_VARCHAR, length=20}],
    [[Version]]
  },
  {reply, #response{status=?STATUS_OK, info=Info}, State1};

default_reply(#request{info = #select{params=[#variable{name = <<"global.max_allowed_packet">>}]}}, _Handler, State) ->
  Info = {
    [#column{name = <<"@@global.max_allowed_packet">>, type=?TYPE_LONG, length=20}],
    [[4194304]]
  },
  {reply, #response{status=?STATUS_OK, info=Info}, State};

default_reply(#request{info = #select{params=[#variable{name = <<"tx_isolation">>, scope = local}]}}, _Handler, State) ->
  Info = {
    [#column{name = <<"@@tx_isolation">>, type=?TYPE_VARCHAR}],
    [[<<"REPEATABLE-READ">>]]
  },
  {reply, #response{status=?STATUS_OK, info=Info}, State};


default_reply(#request{info = {use,Database}}, Handler, State) ->
  {noreply, State1} = Handler:metadata({connect_db,Database}, State),
  {reply, #response {
    status=?STATUS_OK, info = <<"Changed to ", Database/binary>>, status_flags = 2
  }, State1};

default_reply(#request{command = init_db, info = Database}, Handler, State) ->
  {noreply, State1} = Handler:metadata({connect_db,Database}, State),
  {reply, #response {
    status=?STATUS_OK, info = <<"Changed to ", Database/binary>>
  }, State1};


default_reply(#request{info = #show{type = databases}}, Handler, State) ->
  {Databases, State1} = case Handler:metadata(databases, State) of
    {reply, DB, State1_} -> {DB, State1_};
    {noreply, State1_} -> {[<<"myproto">>], State1_}
  end,
  ResponseFields = {
    [#column{name = <<"Database">>, type=?TYPE_VAR_STRING, length=20, schema = <<"information_schema">>, table = <<"SCHEMATA">>, org_table = <<"SCHEMATA">>, org_name = <<"SCHEMA_NAME">>}],
    [ [DB] || DB <- Databases]
  },
  Response = #response{status=?STATUS_OK, info = ResponseFields},
  {reply, Response, State1};


default_reply(#request{info = #show{type = collation}}, _Handler, State) ->
  ResponseFields = {
    [#column{name = <<"Collation">>, type=?TYPE_VAR_STRING, length=20},
    #column{name = <<"Charset">>, type=?TYPE_VAR_STRING, length=20},
    #column{name = <<"Id">>, type=?TYPE_LONG},
    #column{name = <<"Default">>, type=?TYPE_VAR_STRING, length=20},
    #column{name = <<"Compiled">>, type=?TYPE_VAR_STRING, length=20},
    #column{name = <<"Sortlen">>, type=?TYPE_LONG}
    ],
    [ 
      [<<"utf8_bin">>,<<"utf8">>,83,<<"">>,<<"Yes">>,1]
    ]
  },
  {reply, #response{status=?STATUS_OK, info = ResponseFields}, State};



default_reply(#request{info = #show{type = variables}}, Handler, State) ->

  {Version, State1} = case Handler:metadata(version, State) of
    {reply, Vsn, State1_} -> {iolist_to_binary(Vsn), State1_};
    {noreply, State1_} -> {<<"5.6.0">>, State1_}
  end,

  {Mega, Sec, Micro} = erlang:now(),
  Timestamp = iolist_to_binary(io_lib:format("~B.~6..0B", [Mega*1000000 + Sec, Micro])),

  Variables = [
    {<<"sql_mode">>, <<"NO_ENGINE_SUBSTITUTION">>},
    {<<"auto_increment_increment">>, <<"1">>},
    {<<"character_set_client">>, <<"utf8">>},
    {<<"character_set_connection">>, <<"utf8">>},
    {<<"character_set_database">>, <<"utf8">>},
    {<<"character_set_results">>, <<"utf8">>},
    {<<"character_set_server">>, <<"utf8">>},
    {<<"character_set_system">>, <<"utf8">>},
    {<<"date_format">>, <<"%Y-%m-%d">>},
    {<<"datetime_format">>, <<"%Y-%m-%d %H:%i:%s">>},
    {<<"default_storage_engine">>, <<"Flussonic">>},
    {<<"timestamp">>, Timestamp},
    {<<"version">>, Version}
  ],

  ResponseFields = {

    [#column{name = <<"Variable_name">>, type=?TYPE_VAR_STRING, length=20, schema = <<"information_schema">>, table = <<"SCHEMATA">>, org_table = <<"SCHEMATA">>, org_name = <<"SCHEMA_NAME">>},
    #column{name = <<"Value">>, type=?TYPE_VAR_STRING, length=20, schema = <<"information_schema">>, table = <<"SCHEMATA">>, org_table = <<"SCHEMATA">>, org_name = <<"SCHEMA_NAME">>}],

    [tuple_to_list(V) || V <- Variables]

  },
  {reply, #response{status=?STATUS_OK, info = ResponseFields}, State1};


default_reply(#request{info = #select{params = [#function{name = <<"DATABASE">>}]}}, _Handler, State) ->
  ResponseFields = {
    [#column{name = <<"DATABASE()">>, type=?TYPE_VAR_STRING, length=102, flags = 0, decimals = 31}],
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
    [#column{name = <<"Tables_in_", DB/binary>>, type=?TYPE_VAR_STRING, schema = DB, table = <<"TABLE_NAMES">>, 
      org_table = <<"TABLE_NAMES">>, org_name = <<"TABLE_NAME">>, flags = 256, length = 192, decimals = 0}],
    [ [Table] || Table <- Tables]
  },
  Response = #response{status=?STATUS_OK, info = ResponseFields},
  {reply, Response, State1};



default_reply(#request{info = #show{type = fields, from = Table, full = Full}}, Handler, State) ->
  {reply, {_DB, Table, Fields}, State1} = Handler:metadata({fields, Table}, State),
  Header = [
    #column{name = <<"Field">>, org_name = <<"COLUMN_NAME">>, type = ?TYPE_VAR_STRING, table = <<"COLUMNS">>, org_table = <<"COLUMNS">>, schema = <<"information_schema">>, length = 192, flags = 256},
    #column{name = <<"Type">>, org_name = <<"COLUMN_TYPE">>, type = ?TYPE_BLOB, table = <<"COLUMNS">>, org_table = <<"COLUMNS">>, schema = <<"information_schema">>, length = 589815, flags = 256},
    #column{name = <<"Null">>, org_name = <<"IS_NULLABLE">>, type = ?TYPE_VAR_STRING, table = <<"COLUMNS">>, org_table = <<"COLUMNS">>, schema = <<"information_schema">>, length = 9, flags = 256},
    #column{name = <<"Key">>, org_name = <<"COLUMN_KEY">>, type = ?TYPE_VAR_STRING, table = <<"COLUMNS">>, org_table = <<"COLUMNS">>, schema = <<"information_schema">>, length = 9, flags = 256},
    #column{name = <<"Default">>, org_name = <<"COLUMN_DEFAULT">>, type = ?TYPE_BLOB, table = <<"COLUMNS">>, org_table = <<"COLUMNS">>, schema = <<"information_schema">>, length = 589815, flags = 256},
    #column{name = <<"Extra">>, org_name = <<"EXTRA">>, type = ?TYPE_VAR_STRING, table = <<"COLUMNS">>, org_table = <<"COLUMNS">>, schema = <<"information_schema">>, length = 90, flags = 256}
  ] ++ case Full of
    true -> [
      #column{name = <<"Collation">>, org_name = <<"COLLATION_NAME">>, type = ?TYPE_VAR_STRING, table = <<"COLUMNS">>, org_table = <<"COLUMNS">>, schema = <<"information_schema">>, length = 96, flags = 256},
      #column{name = <<"Privileges">>, org_name = <<"PRIVILEGES">>, type = ?TYPE_VAR_STRING, table = <<"COLUMNS">>, org_table = <<"COLUMNS">>, schema = <<"information_schema">>, length = 240, flags = 256},
      #column{name = <<"Comment">>, org_name = <<"COLUMN_COMMENT">>, type = ?TYPE_VAR_STRING, table = <<"COLUMNS">>, org_table = <<"COLUMNS">>, schema = <<"information_schema">>, length = 3072, flags = 256}
    ];
    false -> []
  end,
  Rows = lists:map(fun({Name,Type}) ->
    [atom_to_binary(Name,latin1),
    case Type of string -> <<"varchar(255)">>; _ -> <<"bigint(20)">> end,
    <<"YES">>,
    <<>>,
    undefined,
    <<>>
    ] ++ case Full of
      true -> [case Type of string -> <<"utf8_general_ci">>; _ -> undefined end, <<"select,insert,update,references">>,<<>>];
      false -> []
    end
  end, Fields),
  {reply, #response{status=?STATUS_OK, info = {Header, Rows}}, State1};  



default_reply(#request{info = #show{type = create_table, from = Table}}, Handler, State) ->
  {reply, {_DB, Table, Fields}, State1} = Handler:metadata({fields,Table}, State),
  CreateTable = iolist_to_binary([
    "CREATE TABLE `", Table, "` (\n",
      tl(lists:flatmap(fun({Name,Type}) ->
        [",", "`", atom_to_binary(Name,latin1), "` ", case Type of string -> "varchar(255)"; integer -> "bigint(20)"; boolean -> "tinyint(1)" end, "\n"]
      end, Fields)),
    ")"
  ]),
  Response = {
    [#column{name = <<"Table">>, type = ?TYPE_VAR_STRING, flags = 256, decimals = 31, length = 192},
    #column{name = <<"Create Table">>, type = ?TYPE_VAR_STRING, flags = 256, decimals = 31, length = 3072}],
    [
    [Table, CreateTable]
    ]
  },
  {reply, #response{status=?STATUS_OK, info = Response}, State1};


default_reply(#request{command = field_list, info = Table}, Handler, State) ->
  {reply, {DB, Table, Fields}, State1} = Handler:metadata({fields,Table}, State),
  
  Reply = [#column{schema = DB, table = Table, org_table = Table, name = to_b(Field), org_name = to_b(Field), length = 20,
    type = case Type of string -> ?TYPE_VAR_STRING; integer -> ?TYPE_LONGLONG; boolean -> ?TYPE_TINY end} || {Field,Type} <- Fields],
  {reply, #response{status=?STATUS_OK, info = {Reply}}, State1};    

default_reply(#request{command = ping}, _Handler, State) ->
  {reply, #response{status = ?STATUS_OK, id = 1}, State};

default_reply(_, _Handler, State) ->
  {reply, #response{status=?STATUS_OK}, State}.


to_b(Atom) when is_atom(Atom) -> atom_to_binary(Atom, latin1);
to_b(Bin) when is_binary(Bin) -> Bin.




