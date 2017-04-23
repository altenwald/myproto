-module(my_datatypes).
-author('Manuel Rubio <manuel@altenwald.com>').

-export([
    string_nul_to_binary/1,
    binary_to_varchar/1,
    fix_integer_to_number/2,
    number_to_fix_integer/2,
    var_integer_to_number/1,
    number_to_var_integer/1,
    read_lenenc_string/1
]).

-include("myproto.hrl").


read_lenenc_string(<<16#fc, Len:16/little, Bin:Len/binary, Rest/binary>>) ->
    {Bin, Rest};
read_lenenc_string(<<16#fd, Len:24/little, Bin:Len/binary, Rest/binary>>) ->
    {Bin, Rest};
read_lenenc_string(<<16#fe, Len:64/little, Bin:Len/binary, Rest/binary>>) ->
    {Bin, Rest};
read_lenenc_string(<<Len:8/little, Bin:Len/binary, Rest/binary>>) ->
    {Bin, Rest}.


%% String.NUL

-spec string_nul_to_binary(String :: binary()) -> binary().

string_nul_to_binary(String) ->
    string_nul_to_binary(String, <<>>).

-spec string_nul_to_binary(String :: binary(), Result :: binary()) -> binary().

string_nul_to_binary(<<0,_/binary>>, String) ->
    String;
string_nul_to_binary(<<A:8,Rest/binary>>, String) ->
    string_nul_to_binary(Rest, <<String/binary, A:8>>).

%% Varchars

-spec binary_to_varchar(Binary::binary() | null) -> binary().

binary_to_varchar(null) ->
    ?DATA_NULL;
binary_to_varchar(Binary) ->
    Len = number_to_var_integer(byte_size(Binary)),
    <<Len/binary, Binary/binary>>.

%% Fix Integers

-spec fix_integer_to_number(Size::integer(), Data::integer()) -> integer().

fix_integer_to_number(Size, Data) when is_integer(Size)
                                  andalso is_binary(Data) ->
    BitSize = Size * 8,
    <<Num:BitSize/little>> = Data,
    Num.

-spec number_to_fix_integer(Size::integer(), Data::binary()) -> binary().

number_to_fix_integer(Size, Data) when is_integer(Size)
                                  andalso is_integer(Data) ->
    BitSize = Size * 8,
    <<Data:BitSize/little>>.

%% Var integers

-spec var_integer_to_number(Var::binary()) -> integer().

var_integer_to_number(<<16#fc, Data:16/little>>) -> Data;
var_integer_to_number(<<16#fd, Data:24/little>>) -> Data;
var_integer_to_number(<<16#fe, Data:64/little>>) -> Data;
var_integer_to_number(<<Data:8/little>>) -> Data.

-spec number_to_var_integer(Data::integer()) -> binary().

number_to_var_integer(Data) when is_integer(Data) andalso Data < 251 ->
    <<Data:8>>;
number_to_var_integer(Data) when is_integer(Data) andalso Data < 16#10000 ->
    <<16#fc, Data:16/little>>;
number_to_var_integer(Data) when is_integer(Data) andalso Data < 16#1000000 ->
    <<16#fd, Data:24/little>>;
number_to_var_integer(Data) when is_integer(Data) ->
    <<16#fe, Data:64/little>>.

