-module(nanomysql).
-author('Max Lapshin <max@maxidoors.ru>').

-export([connect/1, execute/2]).

%% @doc
%% connect("mysql://user:password@127.0.0.1/dbname")
%%
connect(URL) ->
  {ok, {mysql, AuthInfo, Host, Port, "/"++DBName}} = http_uri:parse(URL, [{scheme_defaults,[{mysql,3306}]}]),

  [User, Password] = string:tokens(AuthInfo, ":"),

  {ok, Sock} = gen_tcp:connect(Host, Port, [binary,{active,false}]),
  
  {ok, 0, <<ProtoVersion, Rest1/binary>>} = read_packet(Sock),
  [ServerVersion, <<ThreadId:32/little, Scramble1:8/binary, 0, _Caps1:16, Charset, _Status:16, _Caps2:16,
    AuthLen, _Reserve1:10/binary, Scramble2:13/binary, _Rest2/binary>>] = binary:split(Rest1, <<0>>),
  21 = AuthLen,
  <<Scramble:20/binary, _/binary>> = <<Scramble1/binary, Scramble2/binary>>,

  MaxPacket = 16777216,
  Auth = case Password of
    <<>> -> 0;
    "" -> 0;
    _ ->
      Digest1 = crypto:sha(Password),
      SHA = crypto:sha_final(crypto:sha_update(
        crypto:sha_update(crypto:sha_init(), Scramble), 
        crypto:sha(Digest1)
      )),
      [size(SHA), crypto:exor(Digest1, SHA) ]
  end,

  send_packet(Sock, 1, [<<16#4003F7CF:32/little, (MaxPacket-1):32/little, Charset>>, binary:copy(<<0>>, 23), 
    [User, 0], Auth, DBName, 0]),

  {ok, 2, <<0,_/binary>> = _AuthReply} = read_packet(Sock),

  {ok, Sock}.




execute(Query, Sock) ->
  send_packet(Sock, 0, [3, Query]),
  {ok, _, <<Cols>>} = read_packet(Sock),
  Columns = [begin
    {ok, _, FieldBin} = read_packet(Sock),
    {_, B1} = lenenc_str(FieldBin),
    {_, B2} = lenenc_str(B1),
    {_, B3} = lenenc_str(B2),
    {_, B4} = lenenc_str(B3),
    {Field, _} = lenenc_str(B4),
    Field
  end || _ <- lists:seq(1,Cols)],
  {ok, _, <<254, _/binary>>} = read_packet(Sock),

  Rows = read_rows(Sock),
  {ok, {Columns, Rows}}.


read_rows(Sock) ->
  case read_packet(Sock) of
    {ok, _, <<254,_/binary>>} -> [];
    {ok, _, Row} -> [unpack_row(Row)|read_rows(Sock)]
  end.

unpack_row(<<>>) -> [];
unpack_row(<<16#FB, Rest/binary>>) -> [undefined|unpack_row(Rest)];
unpack_row(Bin) -> 
  {Value, Rest} = lenenc_str(Bin),
  [Value|unpack_row(Rest)].



lenenc_str(<<Len, Value:Len/binary, Bin/binary>>) when Len < 251 -> {Value, Bin};
lenenc_str(<<252, Len:16/little, Value:Len/binary, Bin/binary>>) -> {Value, Bin};
lenenc_str(<<253, Len:24/little, Value:Len/binary, Bin/binary>>) -> {Value, Bin}.


print_reply({Columns, Rows}) ->
  io:format("~s\n---\n", [ string:join([io_lib:format("~s",[C]) || C <- Columns], ",")]),
  [io:format("~s\n", [ string:join([io_lib:format("~s",[C]) || C <- Row],",")]) || Row <- Rows],
  io:format("\nok\n").


read_packet(Sock) ->
  {ok, <<Len:24/little, Number>>} = gen_tcp:recv(Sock, 4),
  case gen_tcp:recv(Sock, Len) of
    {ok, <<255, Code:16/little, Error/binary>>} -> {error, {Code, Error}};
    {ok, Bin} -> {ok, Number, Bin}
  end.


send_packet(Sock, Number, Bin) ->
  ok = gen_tcp:send(Sock, [<<(iolist_size(Bin)):24/little, Number>>, Bin]).





