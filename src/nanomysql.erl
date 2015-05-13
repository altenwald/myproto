-module(nanomysql).
-author('Max Lapshin <max@maxidoors.ru>').

-export([connect/1, execute/2, command/3]).
-export([select/2]).

%% @doc
%% connect("mysql://user:password@127.0.0.1/dbname")
%%
connect(URL) ->
  {ok, {mysql, AuthInfo, Host, Port, "/"++DBName, []}} = http_uri:parse(URL, [{scheme_defaults,[{mysql,3306}]}]),

  [User, Password] = case string:tokens(AuthInfo, ":") of
    [U,P] -> [U,P];
    [U] -> [U,""]
  end, 

  {ok, Sock} = gen_tcp:connect(Host, Port, [binary,{active,false}]),
  
  {ok, 0, <<_ProtoVersion, Rest1/binary>>} = read_packet(Sock),
  [_ServerVersion, <<_ThreadId:32/little, Scramble1:8/binary, 0, _Caps1:16, Charset, _Status:16, _Caps2:16,
    AuthLen, _Reserve1:10/binary, Scramble2:13/binary, _Rest2/binary>>] = binary:split(Rest1, <<0>>),
  21 = AuthLen,
  <<Scramble:20/binary, _/binary>> = <<Scramble1/binary, Scramble2/binary>>,

  MaxPacket = 16777216,
  Auth = case Password of
    <<>> -> 0;
    "" -> 0;
    _ ->
      Digest1 = crypto:hash(sha, Password),
      SHA = crypto:hash_final(crypto:hash_update(
        crypto:hash_update(crypto:hash_init(sha), Scramble),
        crypto:hash(sha, Digest1)
      )),
      [size(SHA), crypto:exor(Digest1, SHA) ]
  end,

  % http://dev.mysql.com/doc/internals/en/connection-phase-packets.html#packet-Protocol::HandshakeResponse41
  % CLIENT_CONNECT_WITH_DB = 8
  CapFlags = 16#4003F7CF bor 8,
  send_packet(Sock, 1, [<<CapFlags:32/little, (MaxPacket-1):32/little, Charset>>, binary:copy(<<0>>, 23), 
    [User, 0], Auth, DBName, 0]),

  case read_packet(Sock) of
    {ok, 2, <<0,_/binary>> = _AuthReply} ->
      {ok, Sock};
    {error, _} = Error ->
      Error
  end.


-record(column, {
  name,
  type,
  length
}).

execute(Query, Sock) ->
  command(3, Query, Sock).

select(Query, Sock) ->
  case execute(Query, Sock) of
    {error, Error} ->
      error(Error);
    {ok, {Columns, Rows}} ->
      Names = [binary_to_atom(N,latin1) || {N,_} <- Columns],
      [maps:from_list(lists:zip(Names,Row)) || Row <- Rows]
  end.


command(show_fields, Info, Sock) ->
  command(4, Info, Sock);

command(2 = Cmd, Info, Sock) ->
  send_packet(Sock, 0, [Cmd, Info]),
  {ok, _, <<0, _/binary>>} = read_packet(Sock),
  ok;

command(Cmd, Info, Sock) when is_integer(Cmd) ->
  send_packet(Sock, 0, [Cmd, Info]),
  case read_columns(Sock) of
    {error, Error} ->
      {error, Error};
    ok ->
      {ok, {[],[]}};
    {ok, #{}} = InsertInfo ->
      InsertInfo;
    {_Cols, Columns} -> % response to query
      Rows = read_rows(Columns, Sock),
      {ok, {[{Field,type_name(Type)} || #column{name = Field, type = Type} <- Columns], Rows}};
    Columns ->
      {ok, {[{Field,type_name(Type)} || #column{name = Field, type = Type} <- Columns]}}
  end.


read_columns(Sock) ->
  case read_packet(Sock) of
    {ok, _, <<254, _/binary>>} ->
      [];
    {ok, _, <<0, Packet/binary>>} -> % 0 is STATUS_OK
      {AffectedRows, P1} = varint(Packet),
      {LastInsertId, P2} = varint(P1),
      <<Status:16/little, Warnings:16/little, _Rest/binary>> = P2,
      {ok, #{affected_rows => AffectedRows, last_insert_id => LastInsertId, status => Status, warnings => Warnings}};
    {ok, _, <<Cols>>} -> 
      {Cols, read_columns(Sock)}; % number of columns
    {ok, _, FieldBin} ->
      {_Cat, B1} = lenenc_str(FieldBin), % Catalog
      {_Schema, B2} = lenenc_str(B1),       % schema
      {_Table, B3} = lenenc_str(B2),       % table
      {_OrgTable, B4} = lenenc_str(B3),       % org_table
      {Field, B5} = lenenc_str(B4),   % column name
      {_OrgName, B6} = lenenc_str(B5),       % org_name
      <<16#0c, _Charset:16/little, Length:32/little, Type:8, Flags:16, _Decimals:8, _/binary>> = B6,
      case get(debug) of
        true ->
          io:format("name= ~p, cat= ~p, schema= ~p, table= ~p, org_table= ~p, org_name= ~p, flags=~p, type=~p,decimals=~p,length=~p\n", [
            Field, _Cat, _Schema, _Table, _OrgTable, _OrgName, Flags,Type,_Decimals,Length
            ]);
        _ ->
          ok
      end,
      [#column{name = Field, type = Type, length = Length}|read_columns(Sock)];
    {error, Error} ->
      {error, Error}
  end.





type_name(0) -> decimal;
type_name(1) -> tiny;
type_name(2) -> short;
type_name(3) -> long;
type_name(7) -> timestamp;
type_name(8) -> longlong;
type_name(15) -> varchar;
type_name(16#fc) -> blob;
type_name(16#fd) -> varstring;
type_name(16#fe) -> string;
type_name(T) -> T.




read_rows(Columns, Sock) when is_list(Columns) ->
  case read_packet(Sock) of
    {ok, _, <<254,_/binary>>} -> [];
    {ok, _, Row} -> [unpack_row(Columns, Row)|read_rows(Columns, Sock)]
  end.

unpack_row([], <<>>) -> [];
unpack_row([_|Columns], <<16#FB, Rest/binary>>) -> [undefined|unpack_row(Columns, Rest)];
unpack_row([Column|Columns], Bin) -> 
  {Value, Rest} = lenenc_str(Bin),
  Val = unpack_value(Column, Value),
  [Val|unpack_row(Columns, Rest)].


unpack_value(#column{type = 1}, <<"1">>) -> true;
unpack_value(#column{type = 1}, <<"0">>) -> false;

unpack_value(#column{type = T}, Bin) when 
  T == 1; T == 2; T == 3; T == 8; T == 9; T == 13 ->
  % Len = Length*8,
  % <<Val:Len/little>> = Bin,
  list_to_integer(binary_to_list(Bin));

unpack_value(_, Bin) ->
  Bin.



lenenc_str(<<Len, Value:Len/binary, Bin/binary>>) when Len < 251 -> {Value, Bin};
lenenc_str(<<252, Len:16/little, Value:Len/binary, Bin/binary>>) -> {Value, Bin};
lenenc_str(<<253, Len:24/little, Value:Len/binary, Bin/binary>>) -> {Value, Bin}.


varint(<<16#fe, Data:64/little, Rest/binary>>) -> {Data, Rest};
varint(<<16#fd, Data:32/little, Rest/binary>>) -> {Data, Rest};
varint(<<16#fc, Data:16/little, Rest/binary>>) -> {Data, Rest};
varint(<<Data, Rest/binary>>) -> {Data, Rest}.


read_packet(Sock) ->
  {ok, <<Len:24/little, Sequence>>} = gen_tcp:recv(Sock, 4),
  case gen_tcp:recv(Sock, Len) of
    {ok, <<255, Code:16/little, Error/binary>>} -> {error, {Code, Error}};
    {ok, Bin} -> {ok, Sequence, Bin}
  end.


send_packet(Sock, Number, Bin) ->
  case iolist_size(Bin) of
    Size when Size < 16#ffffff ->
      % io:format("out packet: ~p\n", [iolist_to_binary(Bin)]),
      ok = gen_tcp:send(Sock, [<<Size:24/unsigned-little, Number>>, Bin]);
    _ ->
      <<Command, Rest/binary>> = iolist_to_binary(Bin),
      send_multi_packet(Sock, Number, Command, Rest)
  end.

send_multi_packet(Sock, Number, Command, <<Bin:16#fffffe/binary, Rest/binary>>) ->
  ok = gen_tcp:send(Sock, [<<16#ffffff:24/unsigned-little, Number, Command>>, Bin]),
  send_multi_packet(Sock, Number, Command, Rest);

send_multi_packet(Sock, Number, Command, Bin) ->
  Size = size(Bin) + 1,
  ok = gen_tcp:send(Sock, [<<Size:24/unsigned-little, Number, Command>>, Bin]).







