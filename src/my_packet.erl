-module(my_packet).

-export([encode/1, encode_column/1, decode/1, encode_row/1]).

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
	}) when Code =/= "" ->
	Length = byte_size(Info) + 9,
	<<Length:24/little, Id:8, ?STATUS_ERR:8, Error:16/little, "#", Code:5/binary, Info/binary>>;
encode(#response{
		status=?STATUS_ERR, id=Id, error_code=Error,
		info = Info
	}) ->
	Length = byte_size(Info) + 3,
	<<Length:24/little, Id:8, ?STATUS_ERR:8, Error:16/little, Info/binary>>;
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
	>>.

encode_column(Cols) when is_list(Cols) ->
	lists:foldl(fun(Col, Data) ->
		BinCol = encode_column(Col),
		<<Data/binary, BinCol/binary>>
	end, <<"">>, Cols);
encode_column(#column{
		schema = Schema, table = Table, name = Name,
		charset = Charset, length = Length, type = Type,
		flags = Flags, decimals = Decimals
	}) ->
	SchemaLen = my_datatypes:number_to_var_integer(byte_size(Schema)),
	TableLen = my_datatypes:number_to_var_integer(byte_size(Table)),
	NameLen = my_datatypes:number_to_var_integer(byte_size(Name)),
	FlagsLen = my_datatypes:number_to_var_integer(byte_size(Flags)),
	<<
		3:8, "def", SchemaLen/binary, Schema/binary,
		TableLen/binary, Table/binary, % table
		TableLen/binary, Table/binary, % org_table
		NameLen/binary, Name/binary, % name
		NameLen/binary, Name/binary, % org_name
		16#0c:8, Charset:16/little,
		Length:32/little, Type:8, Flags:16/little,
		Decimals:8,
		FlagsLen/binary, Flags/binary
	>>.

encode_row(Row) ->
	lists:foldl(fun(Cell, Data) ->
		Len = my_datatypes:number_to_var_integer(byte_size(Cell)),
		<<Data/binary, Len/binary, Cell/binary>>
	end, <<"">>, Row).

decode(<<_Length:24/little, _Id:8, Command:8, Info/binary>>) ->
	#request{command=Command, info=Info}.
