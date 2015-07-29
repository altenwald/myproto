-module (test_handler).
-behaviour(gen_myproto).
-include_lib("myproto/include/myproto.hrl").

-export([start_server/1, start_server/2, stop_server/0, stop_server/1, existing_port/1]).
-export([table_columns/1, tables/0]).
-export([check_pass/1, execute/2, terminate/2, metadata/2]).


-define(ERR_WRONG_PASS, {error, <<"Password incorrect!">>}).
-define(ERR_WRONG_USER, {error, <<"No such user!">>}).
-define(ERR_LOGIN_DISABLED, {error, <<"Login disabled">>}).
-define(ERR_INFO(Code, Desc), #response{status=?STATUS_ERR, error_code=Code, info=Desc}).

existing_port(Ref) ->
  Supervisors = supervisor:which_children(ranch_sup),
  case lists:keyfind({ranch_listener_sup,Ref}, 1, Supervisors) of
    {_, Pid, _, _} when is_pid(Pid) ->
      case process_info(Pid) of
        undefined -> undefined;
        _ -> ranch:get_port(Ref)
      end;
    _ -> 
      undefined
  end.

start_server(HostPort) -> start_server(test_sql1, HostPort).
start_server(Name, HostPort) ->
  {Host,Port} = if
    is_number(HostPort) -> {[], HostPort};
    is_binary(HostPort) ->
      [H,P] = binary:split(HostPort, <<":">>),
      {ok, Ip} = inet_parse:address(binary_to_list(H)),
      {[{ip,Ip}],binary_to_integer(P)}
  end,

  case gen_tcp:listen(Port, [binary, {reuseaddr, true}, {active, false}, {backlog, 4096}] ++ Host) of
    {ok, LSock} ->
      ranch:start_listener(Name, 5, ranch_tcp, [{socket, LSock},{max_connections,300}], my_ranch_worker, [?MODULE, []]);
    {error, eaddrinuse}=Err -> 
      lager:info("Cannot start MySQL server on port ~B (in use)", [Port]), Err;
    {error, Error}=Err -> 
      lager:info("Cannot start MySQL server on port ~B (~p)", [Port, Error]), Err
  end.

stop_server() -> stop_server(test_sql1).
stop_server(Name) ->
  my_ranch_worker:stop_server(Name).


-record(my, {
  db
}).

check_pass(#user{name = <<"user">>, password = Pass} = User) ->
  {ok,Pass,#my{}};

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




tables() -> <<"test">>.
table_columns(<<"test">>) -> [{id,string},{name,string},{url,string}].



% $$\                          $$\           
% $$ |                         \__|          
% $$ |      $$$$$$\   $$$$$$\  $$\  $$$$$$$\ 
% $$ |     $$  __$$\ $$  __$$\ $$ |$$  _____|
% $$ |     $$ /  $$ |$$ /  $$ |$$ |$$ /      
% $$ |     $$ |  $$ |$$ |  $$ |$$ |$$ |      
% $$$$$$$$\\$$$$$$  |\$$$$$$$ |$$ |\$$$$$$$\ 
% \________|\______/  \____$$ |\__| \_______|
%                    $$\   $$ |              
%                    \$$$$$$  |              
%                     \______/               

execute(#request{info = #select{tables = [#table{name = Table}]} = Select}, #my{} = State) ->
  TableColumns = table_columns(Table),
  Columns = [N || {N,_} <- TableColumns],
  Rows = [ [{id, <<"1">>}, {name, <<"stream1">>}, {url, <<"rtsp://...">>}] ],
  ResponseColumns = [
    case lists:keyfind(Name,1,table_columns(Table)) of
      false -> {Name, string};
      Col -> Col
    end || Name <- Columns],
  Response = { response_columns(ResponseColumns), [response_row(Row, Columns) || Row <- Rows]},
  {reply, #response{status=?STATUS_OK, info = Response}, State};

  % #select{params = Params, conditions = Conditions1} = Select,
  % Conditions = normalize_condition_types(Handler:table_columns(Table), Conditions1),

  % TableColumns = Handler:table_columns(Table),
  % Columns = case Params of
  %   [#all{}] -> [N || {N,_} <- TableColumns];
  %   [#key{}|_] -> [binary_to_existing_atom(Name,latin1) || #key{name = Name} <- Params];
  %   [#function{name = <<"COUNT">>}] -> [count]
  % end,

  % case Handler:select(Table, Columns, Conditions) of
  %   {error, Code, Desc} -> 
  %     {reply, #response{status=?STATUS_ERR, error_code=Code, info=Desc}, State};
  %   Rows when Columns == [count] andalso is_list(Rows) ->
  %     Response = {
  %       [#column{name = <<"COUNT">>, type = ?TYPE_LONGLONG}],
  %       [[length(Rows)]]
  %     },
  %     {reply, #response{status=?STATUS_OK, info = Response}, State};
  %   {ReplyColumns, Rows} ->
  %     Response = {response_columns(ReplyColumns), Rows},
  %     {reply, #response{status=?STATUS_OK, info = Response}, State};
  %   Rows when is_list(Rows) -> 
  %     ResponseColumns = [case lists:keyfind(Name,1,TableColumns) of
  %       false -> {Name, string};
  %       Col -> Col
  %     end || Name <- Columns],
  %     Response = { response_columns(ResponseColumns), [response_row(Row, Columns) || Row <- Rows]},
  %     {reply, #response{status=?STATUS_OK, info = Response}, State}
  % end;


execute(#request{} = _Request, #my{} = State) ->
  {reply, default, State}.

% execute(#request{command = 'query', info = #system_set{}} = _Request, #my{} = State) ->
%   {reply, default, State};

% execute(#request{command = 'query', info = {use,_}} = _Request, #my{} = State) ->
%   {reply, default, State};

% execute(#request{info = #select{params=[#variable{}]}} = _Request, #my{} = State) ->
%   {reply, default, State};

% execute(#request{command = 'query'}, #my{handler = undefined} = State) ->
%   {reply, #response{status=?STATUS_ERR, error_code=1046, info = <<"No database selected">>}, State};

% execute(#request{command = 'query'} = Request, #my{} = State) ->

%   run_query(Request, State);

% execute(_Default, State) ->
%   % lager:info("default: ~p", [_Default]),
%   {reply, default, State}.


% run_query(#request{info = 'begin'}, #my{} = State) ->
%   {reply, default, State};

% run_query(#request{info = commit}, #my{} = State) ->
%   {reply, default, State};

% run_query(#request{info = rollback}, #my{} = State) ->
%   {reply, default, State};

% run_query(#request{info = #select{params = [#function{name = <<"DATABASE">>}]}}, #my{db = Database} = State) ->
%   {reply, #response{status=?STATUS_OK, info = {
%     [#column{name = <<"database">>, type = ?TYPE_VAR_STRING, length = 20}],
%     [[Database]]
%   }}, State};

% run_query(#request{info = #select{tables = [#table{name = Table}]} = Select}, #my{handler = Handler} = State) ->
%   #select{params = Params, conditions = Conditions1} = Select,
%   Conditions = normalize_condition_types(Handler:table_columns(Table), Conditions1),

%   TableColumns = Handler:table_columns(Table),
%   Columns = case Params of
%     [#all{}] -> [N || {N,_} <- TableColumns];
%     [#key{}|_] -> [binary_to_existing_atom(Name,latin1) || #key{name = Name} <- Params];
%     [#function{name = <<"COUNT">>}] -> [count]
%   end,

%   case Handler:select(Table, Columns, Conditions) of
%     {error, Code, Desc} -> 
%       {reply, #response{status=?STATUS_ERR, error_code=Code, info=Desc}, State};
%     Rows when Columns == [count] andalso is_list(Rows) ->
%       Response = {
%         [#column{name = <<"COUNT">>, type = ?TYPE_LONGLONG}],
%         [[length(Rows)]]
%       },
%       {reply, #response{status=?STATUS_OK, info = Response}, State};
%     {ReplyColumns, Rows} ->
%       Response = {response_columns(ReplyColumns), Rows},
%       {reply, #response{status=?STATUS_OK, info = Response}, State};
%     Rows when is_list(Rows) -> 
%       ResponseColumns = [case lists:keyfind(Name,1,TableColumns) of
%         false -> {Name, string};
%         Col -> Col
%       end || Name <- Columns],
%       Response = { response_columns(ResponseColumns), [response_row(Row, Columns) || Row <- Rows]},
%       {reply, #response{status=?STATUS_OK, info = Response}, State}
%   end;

% run_query(#request{info = #insert{table = #table{name = Table}, values = ValuesSpec}}, #my{handler = Handler, role = admin} = State) ->
%   case erlang:function_exported(Handler, insert, 2) of
%     false ->
%       {reply, #response{status = ?STATUS_ERR, error_code = 1036, info = <<"Table ",Table/binary," is readonly">>}, State};
%     true ->
%       Values = [{K,V} || #set{key = K, value = #value{value = V}} <- ValuesSpec],
%       case Handler:insert(Table, Values) of
%         {error, Code, Desc} ->
%           {reply, #response{status=?STATUS_ERR, error_code=Code, info=Desc}, State};
%         {ok, Id} when is_integer(Id) ->
%           {reply, #response{status=?STATUS_OK, affected_rows = 1, last_insert_id = Id, status_flags = 0, warnings = 0, info = <<>>}, State}
%       end
%   end;

% run_query(#request{info = #insert{}}, #my{} = State) ->
%   {reply, #response{status=?STATUS_ERR, error_code=1227, info= <<"Admin role required for inserting">>}, State};


% run_query(#request{info = #update{table = #table{name = Table}, set = ValuesSpec, conditions = Conditions}}, #my{handler = Handler, role = admin} = State) ->
%   case erlang:function_exported(Handler, update, 3) of
%     false ->
%       {reply, #response{status = ?STATUS_ERR, error_code = 1036, info = <<"Table ",Table/binary," is readonly">>}, State};
%     true ->
%       Values = [{K,V} || #set{key = K, value = #value{value = V}} <- ValuesSpec],
%       case Handler:update(Table, Values, Conditions) of
%         {error, Code, Desc} ->
%           {reply, #response{status=?STATUS_ERR, error_code=Code, info=Desc}, State};
%         {ok, Id} when is_integer(Id) ->
%           {reply, #response{status=?STATUS_OK, affected_rows = 1, last_insert_id = Id, status_flags = 0, warnings = 0, info = <<>>}, State}
%       end
%   end;

% run_query(#request{info = #update{}}, #my{} = State) ->
%   {reply, #response{status=?STATUS_ERR, error_code=1227, info= <<"Admin role required for updating">>}, State};


% run_query(#request{info = #delete{table = #table{name = Table}, conditions = Conditions}}, #my{handler = Handler, role = admin} = State) ->
%   case erlang:function_exported(Handler, delete, 2) of
%     false ->
%       {reply, #response{status = ?STATUS_ERR, error_code = 1036, info = <<"Table ",Table/binary," is readonly">>}, State};
%     true ->
%       case Handler:delete(Table, Conditions) of
%         {error, Code, Desc} ->
%           {reply, #response{status=?STATUS_ERR, error_code=Code, info=Desc}, State};
%         {ok, Id} when is_integer(Id) ->
%           {reply, #response{status=?STATUS_OK, affected_rows = 1, last_insert_id = Id, status_flags = 0, warnings = 0, info = <<>>}, State}
%       end
%   end;

% run_query(#request{info = #delete{}}, #my{} = State) ->
%   {reply, #response{status=?STATUS_ERR, error_code=1227, info= <<"Admin role required for deleting">>}, State};


% run_query(#request{}, #my{} = State) ->
%   {reply, #response{status = ?STATUS_ERR, error_code = 1065, info = <<"Invalid select query">>}, State}.





% normalize_condition_types(Columns, #condition{nexo = OrAnd, op1 = Op1, op2 = Op2}) when OrAnd == nexo_and; OrAnd == nexo_or ->
%   #condition{nexo = OrAnd, 
%     op1 = normalize_condition_types(Columns, Op1),
%     op2 = normalize_condition_types(Columns, Op2)
%   };

% normalize_condition_types(Columns, #condition{nexo = C, op1 = #key{name = Name} = K, op2 = #value{value = V}} = Cond) when V == 1; V == 0->
%   Column = binary_to_existing_atom(Name,latin1),
%   case proplists:get_value(Column, Columns) of
%     boolean when V == 1 -> #condition{nexo = C, op1 = K, op2 = #value{value = true}};
%     boolean when V == 0 -> #condition{nexo = C, op1 = K, op2 = #value{value = false}};
%     _ -> Cond
%   end;

% normalize_condition_types(_Columns, Cond) ->
%   Cond.




response_row(Row, Columns) when is_map(Row) ->
  [maps:get(Column, Row, undefined) || Column <- Columns];

response_row(Row, Columns) when is_list(Row) ->
  [proplists:get_value(Column, Row) || Column <- Columns].


response_columns(Columns) ->
  lists:map(fun
    ({Name,string}) -> #column{name = atom_to_binary(Name,latin1), type = ?TYPE_VAR_STRING, length = 20, org_name = atom_to_binary(Name,latin1)};
    ({Name,boolean}) -> #column{name = atom_to_binary(Name,latin1), type = ?TYPE_TINY, length = 1, org_name = atom_to_binary(Name,latin1)};
    ({Name,integer}) -> #column{name = atom_to_binary(Name,latin1), type = ?TYPE_LONGLONG, length = 20, org_name = atom_to_binary(Name,latin1)}
  end, Columns).


terminate(_Reason,_) -> 
  % lager:info("terminate ~p", [Reason]),
  ok.
