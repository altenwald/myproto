-module(my_packet).

-export([encode/1, decode/1, decode_auth/1]).

-include("../include/myproto.hrl").

encode(#response{
        status=?STATUS_EOF, id=Id, warnings=Warnings, 
        status_flags=StatusFlags
    }) when Warnings == 0 andalso StatusFlags == 0 ->
    <<1:24/little, Id:8, ?STATUS_EOF:8>>;
encode(#response{
        status=?STATUS_EOF, id=Id, warnings=Warnings, 
        status_flags=StatusFlags
    }) ->
    <<5:24/little, Id:8, ?STATUS_EOF:8, Warnings:16/little, StatusFlags:16/little>>;
encode(#response{
        status=?STATUS_ERR, id=Id, error_code=Error,
        error_info = Code, info = Info
    }) when Code =/= <<"">> ->
    Length = byte_size(Info) + 9,
    <<Length:24/little, Id:8, ?STATUS_ERR:8, Error:16/little, "#", Code:5/binary, Info/binary>>;
encode(#response{
        status=?STATUS_ERR, id=Id, error_code=Error,
        info = Info
    }) ->
    Length = byte_size(Info) + 3,
    <<Length:24/little, Id:8, ?STATUS_ERR:8, Error:16/little, Info/binary>>;
encode(#response{
        status=?STATUS_OK, id=Id, info={Cols}
    }) ->
    %% columns
    {IdEof, ColsBin} = encode_column(Cols, Id),
    %% eof
    ColsEof = encode(#response{
        status=?STATUS_EOF, 
        id=IdEof, 
        status_flags=?SERVER_STATUS_AUTOCOMMIT
    }),
    <<ColsBin/binary, ColsEof/binary>>;
encode(#response{
        status=?STATUS_OK, id=Id, info={Cols, Rows}
    }) ->
    %% Column account
    ColLen = length(Cols),
    Head = <<1:24/little, Id:8, ColLen:8>>,
    %% columns
    {IdEof, ColsBin} = encode_column(Cols, Id+1),
    %% eof
    ColsEof = encode(#response{
        status=?STATUS_EOF, 
        id=IdEof, 
        status_flags=?SERVER_STATUS_AUTOCOMMIT
    }),
    %% rows
    {IdEnd, RowsPack} = encode_rows(Rows, Cols, IdEof+1),
    %% eof
    RowsEof = encode(#response{
        status=?STATUS_EOF,
        id=IdEnd,
        status_flags=?SERVER_STATUS_AUTOCOMMIT
    }),
    <<Head/binary, ColsBin/binary, ColsEof/binary, RowsPack/binary, RowsEof/binary>>;
encode(#response{
        status=?STATUS_OK, id=Id, info = Info,
        affected_rows = AffectedRows, last_insert_id = LastInsertId,
        status_flags = StatusFlags, warnings = Warnings
    }) ->
    BinAffectedRows = my_datatypes:number_to_var_integer(AffectedRows),
    BinLastInsertId = my_datatypes:number_to_var_integer(LastInsertId),
    Length = byte_size(BinAffectedRows) + byte_size(BinLastInsertId) + byte_size(Info) + 5,
    <<
        Length:24/little, Id:8, ?STATUS_OK:8, BinAffectedRows/binary, 
        BinLastInsertId/binary, StatusFlags:16/little, Warnings:16/little,
        Info/binary
    >>;
encode(#response{
        status=?STATUS_HELLO, id=Id, info=Hash
    }) ->
    20 == size(Hash) orelse error({invalid_hash_size,size(Hash),need,20}),
    ServerSign = case application:get_env(myproto, server_sign) of
        {ok, SS} when is_binary(SS) -> SS;
        {ok, SS} when is_list(SS) -> list_to_binary(SS);
        undefined -> ?SERVER_SIGN
    end,
    Caps = 
        ?CLIENT_PLUGIN_AUTH bor %% PLAIN AUTH
        ?CLIENT_PROTOCOL_41 bor %% PROTOCOL 4.1
        ?CLIENT_SECURE_CONNECTION bor %% for mysql_native_password
        0,
    <<CapsLow:16/little, CapsUp:16/little>> = <<Caps:32/little>>,
    % <<IntHash:160/little-unsigned-integer>> = Hash,
    <<Auth1:8/binary, Auth2/binary>> = Hash, %<<IntHash:160/little-unsigned-integer>>,
    LenAuth = 21,
    StatusFlags = 
        ?SERVER_STATUS_AUTOCOMMIT bor
        0,
    Charset = ?UTF8_GENERAL_CI,
    Info = <<
        ServerSign/binary, 0:8, Id:32/little, 
        Auth1/binary, 0:8, CapsLow:16/little,
        Charset:8, StatusFlags:16/little, 
        CapsUp:16/little, LenAuth:8, 0:80,
        Auth2/binary, 0:8, "mysql_native_password", 0:8
    >>,
    Length = byte_size(Info) + 1,
    << Length:24/little, 0:8, ?STATUS_HELLO:8, Info/binary >>.

encode_column(Cols, Id) when is_list(Cols) ->
    lists:foldl(fun(Col, {NewId, Data}) ->
        BinCol = encode_column(Col, NewId),
        {NewId+1, <<Data/binary, BinCol/binary>>}
    end, {Id, <<"">>}, Cols);
encode_column(#column{
        schema = Schema, table = Table, name = Name,
        charset = Charset, length = L, type = Type,
        flags = Flags, decimals = Decimals, org_name = ON
    }=_Column, Id) when is_binary(Schema), is_binary(Table), is_binary(Name),
    is_integer(Charset), is_integer(Type), is_integer(Flags), 
    is_integer(Decimals) ->
    SchemaLen = my_datatypes:number_to_var_integer(byte_size(Schema)),
    TableLen = my_datatypes:number_to_var_integer(byte_size(Table)),
    NameLen = my_datatypes:number_to_var_integer(byte_size(Name)),
    {OrgNameLen, OrgName} = case ON of 
        undefined -> {NameLen, bin_to_upper(Name)};
        ON -> {my_datatypes:number_to_var_integer(byte_size(ON)), ON}
    end,
    Length = case {Type, L} of 
        _ when is_integer(L) -> L;
        {?TYPE_DATETIME,undefined} -> 16#13;
        {?TYPE_LONGLONG,undefined} -> 16#15;
        {_,undefined} -> 0
    end,
    % lager:debug("Column to encode: ~p~n", [Column]),
    Payload = <<
        3:8, "def", 
        SchemaLen/binary, Schema/binary,
        TableLen/binary, Table/binary, % table
        TableLen/binary, Table/binary, % org_table
        NameLen/binary, Name/binary, % name
        OrgNameLen/binary, OrgName/binary, % org_name
        16#0c:8, Charset:16/little,
        Length:32/little, Type:8, Flags:16/little,
        Decimals:8/little,
        0:16/little
    >>,
    PayloadLen = byte_size(Payload),
    <<PayloadLen:24/little, Id:8, Payload/binary>>.

encode_rows(Rows, Cols, Id) ->
    lists:foldl(fun(Values, {NewId, Data}) ->
        Payload = lists:foldl(fun({#column{type = Type, name = Name}, Cell}, Binary) ->
            Cell1 = case Type of
                _ when Cell == undefined -> undefined;
                _ when Cell == true -> <<"1">>;
                _ when Cell == false -> <<"0">>;
                T when (T == ?TYPE_TINY orelse
                       T == ?TYPE_SHORT orelse
                       T == ?TYPE_LONG orelse
                       T == ?TYPE_LONGLONG orelse
                       T == ?TYPE_INT24 orelse
                       T == ?TYPE_YEAR) andalso is_integer(Cell) -> integer_to_binary(Cell);
                _ when is_binary(Cell) -> Cell;
                _ -> error({cannot_encode,Name,Type,Cell})
            end,
            CellEnc = case Cell of
                undefined -> ?DATA_NULL;
                _ -> my_datatypes:binary_to_varchar(Cell1)
            end,
            <<Binary/binary, CellEnc/binary>>
        end, <<"">>, lists:zip(Cols,Values)),
        Length = byte_size(Payload),
        {NewId+1, <<Data/binary, Length:24/little, NewId:8, Payload/binary>>}
    end, {Id, <<"">>}, Rows).



-spec decode_auth(binary()) -> {ok, user(), binary()} | {more, binary()}.


decode_auth(<<Length:24/little, 1:8, Bin:Length/binary, Rest/binary>>) ->
    {ok, decode_auth0(Bin), Rest};

decode_auth(<<Length:24/little, 1:8, Bin/binary>>) ->
    {more, size(Bin) - Length};

decode_auth(<<Bin/binary>>) ->
    {more, 4 - size(Bin)}.


decode_auth0(<<CapsFlag:32/little, _MaxPackSize:32/little, Charset:8, _Reserved:23/binary, Info0/binary>>) ->
    Caps = unpack_caps(CapsFlag),
    {User, Info1} = unpack_zero(Info0),

    {Password,Info2} = case proplists:get_value(auth_lenenc_client_data,Caps) of
        true -> my_datatypes:read_lenenc_string(Info1);
        _ ->
            case proplists:get_value(secure_connection, Caps) of
                true ->
                    <<PassLen, Pass:PassLen/binary, R/binary>> = Info1,
                    {Pass, R};
                false ->
                    unpack_zero(Info1)
            end
    end,

    HasPluginAuth = proplists:get_value(plugin_auth, Caps),
    {DB, Info3} = case proplists:get_value(connect_with_db, Caps) of
        % For some strange reasons mysql 5.0.6 violates protocol and doesn't send db name
        % http://dev.mysql.com/doc/internals/en/connection-phase-packets.html#packet-Protocol::HandshakeResponse41
        % so we write here a dirty hack for pymysql
        true when HasPluginAuth == undefined andalso size(Info2) > 0 ->
            unpack_zero(Info2);
        _ ->
            {undefined, Info2}
    end,
    {Plugin, _Info4} = case proplists:get_value(plugin_auth, Caps) of
        true ->
            unpack_zero(Info3);
        _ ->
            {undefined, Info3}
    end,
    UserData = #user{
        name=User, 
        password=Password, 
        plugin=Plugin,
        capabilities=Caps,
        database=DB,
        charset=Charset
    },
    #request{command=?COM_AUTH, info=UserData}.

unpack_zero(String) ->
    [B1, B2] = binary:split(String, <<0>>),
    {B1, B2}.


unpack_caps(Flag) ->
    Caps = [
        {?CLIENT_LONG_PASSWORD,long_password},
        {?CLIENT_FOUND_ROWS,found_rows},
        {?CLIENT_LONG_FLAG, long_flag},
        {?CLIENT_CONNECT_WITH_DB, connect_with_db},
        {?CLIENT_NO_SCHEMA, no_schema},
        {?CLIENT_COMPRESS, compress},
        {?CLIENT_ODBC, odbc},
        {?CLIENT_LOCAL_FILES, local_files},
        {?CLIENT_IGNORE_SPACE, ignore_space},
        {?CLIENT_PROTOCOL_41, protocol_41},
        {?CLIENT_INTERACTIVE, interactive},
        {?CLIENT_SSL, ssl},
        {?CLIENT_IGNORE_SIGPIPE, ignore_sigpipe},
        {?CLIENT_TRANSACTIONS, transactions},
        {?CLIENT_SECURE_CONNECTION, secure_connection},
        {?CLIENT_MULTI_STATEMENTS, multi_statements},
        {?CLIENT_MULTI_RESULTS, multi_results},
        {?CLIENT_PS_MULTI_RESULTS, ps_multi_results},
        {?CLIENT_PLUGIN_AUTH, plugin_auth},
        {?CLIENT_CONNECT_ATTRS, connect_attrs},
        {?CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA, auth_lenenc_client_data}
    ],
    lists:flatmap(fun({I,Tag}) ->
        case Flag band I of
            0 -> [];
            _ -> [Tag]
        end
    end, Caps).


-spec decode(binary()) -> {ok, response(), binary()} | {more, binary()}.

decode(<<Length:24/little, Id, Bin:Length/binary, Rest/binary>>) ->
    % lager:info("incoming packet: ~p", [Bin]),
    {ok, decode0(Length, Id, Bin), Rest};

decode(<<Length:24/little, _Id, Bin/binary>>) ->
    {more, Length - size(Bin)};

decode(<<Bin/binary>>) ->
    {more, 4 - size(Bin)}.

decode0(_, Id, <<?COM_FIELD_LIST:8, Info/binary>>) ->
    #request{
        command=?COM_FIELD_LIST, 
        id=Id, 
        info=my_datatypes:string_nul_to_binary(Info)
    };
decode0(16#ffffff, Id, <<Command:8, Info/binary>>) ->
    #request{
        command=Command,
        id=Id,
        info=Info, 
        continue=true
    };
decode0(_, Id, <<Command:8, Info/binary>>) ->
    #request{
        command=Command,
        id=Id,
        info=Info,
        continue=false
    }.

bin_to_upper(Lower) when is_binary(Lower) ->
    << <<(
        if 
            X >= $a andalso X =< $z -> X-32; 
            true -> X end
    )/integer>> || <<X>> <= Lower >>.
