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
-export([decode/2]).
-export([hello/2, ok/1]).
-export([next_packet/1]).


-record(my, {
  connection_id :: non_neg_integer(), %% connection id
  hash ::binary(),                    %% hash for auth
  state :: auth | normal,
  buffer :: undefined | binary(),
  socket :: undefined | inet:socket(), %% When socket is set, client will send data
  id = 1 :: non_neg_integer()
}).

-type my() :: #my{}.


init() ->
  #my{}.

init(Socket) ->
  #my{socket = Socket}.


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
  send_or_reply(my_packet:encode(Hello), My#my{connection_id = ConnectionId, hash = Hash, state = auth, id = 2}).


send_or_reply(Bin, #my{socket = undefined} = My) ->
  {ok, Bin, My};

send_or_reply(Bin, #my{socket = Socket} = My) ->
  ok = gen_tcp:send(Socket, Bin),
  {ok, My}.




-spec ok(my()) -> {ok, Reply::binary(), my()}.

ok(#my{id = Id} = My) ->
  Response = #response{
    status = ?STATUS_OK,
    status_flags = ?SERVER_STATUS_AUTOCOMMIT,
    id = Id
  },
  send_or_reply(my_packet:encode(Response), My#my{id = Id+1}).



-spec decode(binary(), my()) -> {ok, Reply::response() | user(), my()}.

decode(Bin, #my{buffer = Buffer} = My) when size(Buffer) > 0 andalso size(Bin) > 0 ->
  decode(My#my{buffer = <<Bin/binary, Buffer/binary>>});

decode(Bin, #my{} = My) ->
  decode(My#my{buffer = Bin}).



-spec decode(my()) -> {ok, Reply::response() | user(), my()}.
decode(#my{buffer = Bin, state = auth, hash = Hash} = My) when size(Bin) > 4 ->
  case my_packet:decode_auth(Bin) of
    {more, _} ->
      {more, My};
    {ok, #request{info = #user{} = User} = Req, Rest} ->
      {ok, Req#request{info = User#user{server_hash = Hash}}, My#my{state = normal, buffer = Rest}}
  end;

decode(#my{buffer = Bin, state = normal} = My) when size(Bin) > 4 ->
  case my_packet:decode_auth(Bin) of
    {more, _} ->
      {more, My};
    {ok, Reply, Rest} ->
      {ok, Reply, My#my{buffer = Rest}}
  end;

decode(#my{} = My) ->
  {more, My}.






next_packet(#my{buffer = undefined, socket = Socket, state = State} = My) when Socket =/= undefined ->
  case gen_tcp:recv(Socket, 4) of
    {ok, <<Length:24/little, _>> = Header} ->
      case gen_tcp:recv(Socket, Length) of
        {ok, Bin} when size(Bin) == Length andalso State == normal ->
          {ok, Packet, <<>>} = my_packet:decode(<<Header/binary, Bin/binary>>),
          {ok, Packet, My};
        {ok, Bin} when size(Bin) == Length andalso State == auth ->
          {ok, Packet, <<>>} = my_packet:decode_auth(<<Header/binary, Bin/binary>>),
          {ok, Packet, My#my{state = normal}};
        {error, _} = Error ->
          Error
      end;
    {error, _} = Error ->
      Error
  end.





