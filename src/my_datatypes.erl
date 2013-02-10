-module(my_datatypes).

-export([
	binary_to_varchar/1,
	fix_integer_to_number/2, number_to_fix_integer/2,
	var_integer_to_number/1, number_to_var_integer/1
]).

-include("../include/myproto.hrl").

%% Varchars

binary_to_varchar(Binary) ->
	Len = number_to_var_integer(byte_size(Binary)),
	<<Len/binary, Binary/binary>>.

%% Fix Integers

fix_integer_to_number(Size, Data) when is_integer(Size) andalso is_integer(Data) ->
	BitSize = Size * 8,
	<<Num:BitSize/little>> = Data,
	Num.

number_to_fix_integer(Size, Data) when is_integer(Size) andalso is_binary(Data) ->
	BitSize = Size * 8,
	<<Data:BitSize/little>>.

%% Var integers

var_integer_to_number(<<16#fc, Data:16/little>>) -> Data;
var_integer_to_number(<<16#fd, Data:24/little>>) -> Data;
var_integer_to_number(<<16#fe, Data:64/little>>) -> Data;
var_integer_to_number(<<Data:8/little>>) -> Data.

number_to_var_integer(Data) when is_integer(Data) andalso Data < 251 ->
	<<Data:8>>;
number_to_var_integer(Data) when is_integer(Data) andalso Data < 16#10000 ->
	<<16#fc, Data:16/little>>;
number_to_var_integer(Data) when is_integer(Data) andalso Data < 16#1000000 ->
	<<16#fd, Data:24/little>>;
number_to_var_integer(Data) when is_integer(Data) ->
	<<16#fe, Data:64/little>>.

