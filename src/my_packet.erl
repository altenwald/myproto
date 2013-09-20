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
    {IdEnd, RowsPack} = encode_row(Rows, IdEof+1),
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
        charset = Charset, length = Length, type = Type,
        flags = Flags, decimals = Decimals
    }, Id) ->
    SchemaLen = my_datatypes:number_to_var_integer(byte_size(Schema)),
    TableLen = my_datatypes:number_to_var_integer(byte_size(Table)),
    NameLen = my_datatypes:number_to_var_integer(byte_size(Name)),
    Payload = <<
        3:8, "def", 
        SchemaLen/binary, Schema/binary,
        TableLen/binary, Table/binary, % table
        TableLen/binary, Table/binary, % org_table
        NameLen/binary, Name/binary, % name
        NameLen/binary, Name/binary, % org_name
        16#0c:8, Charset:16/little,
        Length:32/little, Type:8, Flags:16/little,
        Decimals:8/little,
        0:16/little
    >>,
    PayloadLen = byte_size(Payload),
    <<PayloadLen:24/little, Id:8, Payload/binary>>.

encode_row(Rows, Id) ->
    lists:foldl(fun(Cols, {NewId, Data}) ->
        Payload = lists:foldl(fun(Cell, Col) ->
            CellEnc = my_datatypes:binary_to_varchar(Cell), 
            <<Col/binary, CellEnc/binary>>
        end, <<"">>, Cols),
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


decode_auth0(<<Caps:32/little, _MaxPackSize:32/little, Charset:8, _Reserved:23/binary, Info/binary>>) ->
    case binary:split(Info, <<0>>) of 
        [User, <<20:8, Password:20/binary, PlugIn/binary>>] ->
            UserData = #user{
                name=User, 
                password=Password, 
                plugin=PlugIn, 
                capabilities=Caps, 
                charset=Charset
            };
        [User, <<0:8, PlugIn/binary>>] ->
            UserData = #user{
                name=User,
                password=undefined,
                plugin=PlugIn,
                capabilities=Caps,
                charset=Charset
            }
    end,
    #request{command=?COM_AUTH, info=UserData}.



-spec decode(binary()) -> {ok, response(), binary()} | {more, binary()}.

decode(<<Length:24/little, Id, Bin:Length/binary, Rest/binary>>) ->
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
