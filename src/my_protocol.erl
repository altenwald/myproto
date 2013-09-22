% @doc
% This is a server-side protocol implementation
%
% When server process receives socket, it MUST first call hello:
%
%   handle_info({socket, Socket}, State) ->
%     {ok, Hello, My} = my_protocol:hello(ConnectionId), % here ConnectionId is a thread id
%     ok = gen_tcp:send(Socket, Hello),
%     inet:setopts(Socket, [{active,once}]),
%     {noreply, State#state{my = My}};
%
-module(my_protocol).
-include("../include/myproto.hrl").
-author('Max Lapshin <max@maxidoors.ru>').

-export([init/0, init/1]).
-export([decode/2, decode/1, buffer_bytes/2]).
-export([send_or_reply/2, hello/2, ok/1, error/2, error/3]).
-export([next_packet/1]).


-record(my, {
  connection_id :: non_neg_integer(), %% connection id
  hash ::binary(),                    %% hash for auth
  state :: auth | normal,
  parse_query = true :: boolean(),    %% parse query or not
  buffer :: undefined | binary(),
  query_buffer = <<>> :: binary(),    %% buffer for long queries
  socket :: undefined | inet:socket(), %% When socket is set, client will send data
  id = 1 :: non_neg_integer()
}).

-type my() :: #my{}.


init() ->
  init([]).

init(Options) ->
  Socket = proplists:get_value(socket, Options),
  ParseQuery = proplists:get_value(parse_query, Options, true),
  #my{socket = Socket, parse_query = ParseQuery}.


-spec hello(ConnectionId::non_neg_integer(), my()) -> {ok, Bin::iodata(), State::my()} | {ok, my()}.

hello(ConnectionId, #my{} = My) when is_integer(ConnectionId) ->
  Hash = list_to_binary(
      lists:map(fun
          (0) -> 1; 
          (X) -> X 
      end, binary_to_list(
          crypto:rand_bytes(20)
      ))
  ),
  Hello = #response{
      id=ConnectionId, 
      status=?STATUS_HELLO, 
      info=Hash
  },
  send_or_reply(Hello, My#my{connection_id = ConnectionId, hash = Hash, state = auth, id = 2}).




send_or_reply(ok, #my{} = My) ->
  ok(My);

send_or_reply(#response{id = 0} = Response, #my{id = Id} = My) ->
  send_or_reply(my_packet:encode(Response#response{id = Id}), My#my{id = Id+1});

send_or_reply(#response{} = Response, #my{} = My) ->
  send_or_reply(my_packet:encode(Response), My);

send_or_reply(Bin, #my{socket = undefined} = My) when is_binary(Bin) ->
  {ok, Bin, My};

send_or_reply(Bin, #my{socket = Socket} = My) when is_binary(Bin) ->
  % lager:info("outgoing ~p", [Bin]),
  ok = gen_tcp:send(Socket, Bin),
  {ok, My}.




-spec ok(my()) -> {ok, Reply::binary(), my()} | {ok, my()}.

ok(#my{} = My) ->
  Response = #response{
    status = ?STATUS_OK,
    status_flags = ?SERVER_STATUS_AUTOCOMMIT
  },
  send_or_reply(Response, My).



-spec error(Reason::binary(), my()) -> {ok, Reply::binary(), my()} | {ok, my()}.

error(Reason, #my{} = My) when is_binary(Reason) ->
  error(1045, Reason, My).


-spec error(Code::non_neg_integer(), Reason::binary(), my()) -> {ok, Reply::binary(), my()} | {ok, my()}.

error(Code, Reason, #my{} = My) when is_integer(Code), is_binary(Reason) ->
  Response = #response{
    status = ?STATUS_ERR,
    error_code = Code,
    info = Reason
  },
  send_or_reply(Response, My).





-spec decode(binary(), my()) -> {ok, Reply::request(), my()}.

decode(Bin, #my{} = My) ->
  My1 = buffer_bytes(Bin, My),
  decode(My1).


-spec buffer_bytes(binary(), my()) -> my().

buffer_bytes(Bin, #my{buffer = Buffer} = My) when size(Buffer) > 0 andalso size(Bin) > 0 ->
  My#my{buffer = <<Bin/binary, Buffer/binary>>};

buffer_bytes(Bin, #my{} = My) ->
  My#my{buffer = Bin}.



-spec decode(my()) -> {ok, Reply::request(), my()}.
decode(#my{buffer = Bin, state = auth, hash = Hash} = My) when size(Bin) > 4 ->
  case my_packet:decode_auth(Bin) of
    {more, _} ->
      {more, My};
    {ok, #request{info = #user{} = User} = Req, Rest} ->
      {ok, Req#request{info = User#user{server_hash = Hash}}, My#my{state = normal, buffer = Rest}}
  end;

decode(#my{buffer = Bin, state = normal, parse_query = ParseQuery, query_buffer = QB} = My) when size(Bin) > 4 ->
  case my_packet:decode(Bin) of
    {more, _} ->
      {more, My};
    {ok, #request{continue = true, info = Info}, Rest} ->
      QB1 = case QB of
        <<>> -> Info;
        _ -> <<QB/binary, Info/binary>>
      end,
      decode(My#my{buffer = Rest, query_buffer = QB1});
    {ok, #request{continue = false, info = Info, command = CommandCode, id = Id} = Packet, Rest} ->
      Query = case QB of
        <<>> -> Info;
        _ -> <<QB/binary, Info/binary>>
      end,
      Command = case CommandCode of
        ?COM_SLEEP -> sleep;
        ?COM_QUIT -> quit;
        ?COM_INIT_DB -> init_db;
        ?COM_QUERY -> 'query';
        ?COM_FIELD_LIST -> field_list;
        ?COM_CREATE_DB -> create_db;
        ?COM_DROP_DB -> drop_db;
        ?COM_REFRESH -> refresh;
        ?COM_SHUTDOWN -> shutdown;
        ?COM_STATISTICS -> statistics;
        Else -> Else
      end,

      Packet1 = case Command of
        'query' when ParseQuery ->
          SQL = case mysql_proto:parse(Query) of
            {fail,Expected} -> {parse_error, {fail,Expected}, Info};
            % {_, Extra,Where} -> {parse_error, {Extra, Where}, Info};
            Parsed -> Parsed
          end,
          Packet#request{command = Command, info = SQL};
        _ ->
          Packet#request{command = Command, info = Query}
      end,
      {ok, Packet1, My#my{buffer = Rest, id = Id + 1}}
  end;

decode(#my{} = My) ->
  {more, My}.






next_packet(#my{buffer = Buffer, socket = Socket} = My) when Socket =/= undefined andalso (Buffer == undefined orelse Buffer == <<>>) ->
  case gen_tcp:recv(Socket, 4) of
    {ok, <<Length:24/unsigned-little, _>> = Header} ->
      case gen_tcp:recv(Socket, Length) of
        {ok, Bin} when size(Bin) == Length ->
          case decode(<<Header/binary, Bin/binary>>, My) of
            {more, My1} -> next_packet(My1);
            {ok, Response, My1} -> {ok, Response, My1}
          end;
        {error, _} = Error ->
          Error
      end;
    {error, _} = Error ->
      Error
  end.





