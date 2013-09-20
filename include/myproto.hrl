-define(SERVER_SIGN, <<"5.5-myproto">>).

%% status flags
-define(SERVER_STATUS_IN_TRANS, 16#0001).
-define(SERVER_STATUS_AUTOCOMMIT, 16#0002).
-define(SERVER_MORE_RESULTS_EXISTS, 16#0008).
-define(SERVER_STATUS_NO_GOOD_INDEX_USED, 16#0010).
-define(SERVER_STATUS_NO_INDEX_USED, 16#0020).
-define(SERVER_STATUS_CURSOR_EXISTS, 16#0040).
-define(SERVER_STATUS_LAST_ROW_SENT, 16#0080).
-define(SERVER_STATUS_DB_DROPPED, 16#0100).
-define(SERVER_STATUS_NO_BACKSLASH_ESCAPES, 16#0200).
-define(SERVER_STATUS_METADATA_CHANGED, 16#0400).
-define(SERVER_QUERY_WAS_SLOW, 16#0800).
-define(SERVER_PS_OUT_PARAMS, 16#1000).

% commands
-define(COM_SLEEP, 0).
-define(COM_QUIT, 1).
-define(COM_INIT_DB, 2).
-define(COM_QUERY, 3).
-define(COM_FIELD_LIST, 4).
-define(COM_CREATE_DB, 5).
-define(COM_DROP_DB, 6).
-define(COM_REFRESH, 7).
-define(COM_SHUTDOWN, 8).
-define(COM_STATISTICS, 9).
-define(COM_PROCESS_INFO, 10).
-define(COM_CONNECT, 11).
-define(COM_PROCESS_KILL, 12).
-define(COM_DEBUG, 13).
-define(COM_PING, 14).
-define(COM_TIME, 15).
-define(COM_DELAYED_INSERT, 16).
-define(COM_CHANGE_USER, 17).
-define(COM_BINLOG_DUMP, 18).
-define(COM_TABLE_DUMP, 19).
-define(COM_CONNECT_OUT, 20).
-define(COM_REGISTER_SLAVE, 21).
-define(COM_STMT_PREPARE, 22).
-define(COM_STMT_EXECUTE, 23).
-define(COM_STMT_SEND_LONG_DATA, 24).
-define(COM_STMT_CLOSE, 25).
-define(COM_STMT_RESET, 26).
-define(COM_SET_OPTION, 27).
-define(COM_STMT_FETCH, 28).
-define(COM_DAEMON, 29).
-define(COM_BINLOG_DUMP_GTID, 30).
-define(COM_AUTH, auth).

% response status
-define(STATUS_OK, 0).
-define(STATUS_HELLO, 16#0A).
-define(STATUS_EOF, 16#FE).
-define(STATUS_ERR, 16#FF).

% data types
-define(TYPE_DECIMAL, 0).
-define(TYPE_TINY, 1).
-define(TYPE_SHORT, 2).
-define(TYPE_LONG ,3).
-define(TYPE_FLOAT ,4).
-define(TYPE_DOUBLE ,5).
-define(TYPE_NULL ,6).
-define(TYPE_TIMESTAMP ,7).
-define(TYPE_LONGLONG, 8).
-define(TYPE_INT24, 9).
-define(TYPE_DATE, 10).
-define(TYPE_TIME, 11).
-define(TYPE_DATETIME, 12).
-define(TYPE_YEAR, 13).
-define(TYPE_NEWDATE, 14).
-define(TYPE_VARCHAR, 15).
-define(TYPE_BIT, 16).
-define(TYPE_NEWDECIMAL, 16#f6).
-define(TYPE_ENUM, 16#f7).
-define(TYPE_SET, 16#f8).
-define(TYPE_TINY_BLOB, 16#f9).
-define(TYPE_MEDIUM_BLOB, 16#fa).
-define(TYPE_LONG_BLOB, 16#fb).
-define(TYPE_BLOB, 16#fc).
-define(TYPE_VAR_STRING, 16#fd).
-define(TYPE_STRING, 16#fe).
-define(TYPE_GEOMETRY, 16#ff).

% charsets
-define(BIG5_CHINESE_CI, 1).
-define(LATIN2_CZECH_CS, 2).
-define(DEC8_SWEDISH_CI, 3).
-define(CP850_GENERAL_CI, 4).
-define(LATIN1_GERMAN1_CI, 5).
-define(HP8_ENGLISH_CI, 6).
-define(KOI8R_GENERAL_CI, 7).
-define(LATIN1_SWEDISH_CI, 8).
-define(LATIN2_GENERAL_CI, 9).
-define(SWE7_SWEDISH_CI, 10).
-define(UTF8_GENERAL_CI, 33).
-define(BINARY, 63).

% capabilities
-define(CLIENT_LONG_PASSWORD, 1).
-define(CLIENT_FOUND_ROWS, 2).
-define(CLIENT_LONG_FLAG, 4).
-define(CLIENT_CONNECT_WITH_DB, 8).
-define(CLIENT_NO_SCHEMA, 16#10).
-define(CLIENT_COMPRESS, 16#20).
-define(CLIENT_ODBC, 16#40).
-define(CLIENT_LOCAL_FILES, 16#80).
-define(CLIENT_IGNORE_SPACE, 16#100).
-define(CLIENT_PROTOCOL_41, 16#200).
-define(CLIENT_INTERACTIVE, 16#400).
-define(CLIENT_SSL, 16#800).
-define(CLIENT_IGNORE_SIGPIPE, 16#1000).
-define(CLIENT_TRANSACTIONS, 16#2000).
-define(CLIENT_RESERVED, 16#4000).
-define(CLIENT_SECURE_CONNECTION, 16#8000).
-define(CLIENT_MULTI_STATEMENTS, 16#10000).
-define(CLIENT_MULTI_RESULTS, 16#20000).
-define(CLIENT_PS_MULTI_RESULTS, 16#40000).
-define(CLIENT_PLUGIN_AUTH, 16#80000).
-define(CLIENT_CONNECT_ATTRS, 16#100000).
-define(CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA, 16#200000).

-include("sql.hrl").

-record(user, {
	name :: binary(),
	password :: binary(),
	capabilities :: integer(),
	plugin :: binary(),
	charset :: binary(),
	server_hash :: binary()
}).

-type user() :: #user{}.

-record(request, {
	command :: integer(),
	info :: string() | sql() | user(),
	continue = false :: boolean(),
	id = 0 :: integer()
}).

-type request() :: #request{}.

-record(response, {
	status = 0 :: integer(),
	id = 0 :: integer(),
	affected_rows = 0 :: integer(), %% as var_integer
	last_insert_id = 0 :: integer(), %% as var_integer
	status_flags = 0 :: integer(),
	warnings = 0 :: integer(), %% only with protocol 4.1
	info = <<>> :: binary(),
	error_code = 0 :: integer(),
	error_info = <<>> :: binary()
}).

-type response() :: #response{}.

-record(column, {
	schema = <<>> :: binary(),
	table = <<>> :: binary(),
	name :: binary(),
	charset = ?UTF8_GENERAL_CI :: integer(),
	length :: integer(),
	type :: integer(),
	flags = 0 :: integer(),
	decimals = 0 :: integer(),
	default = <<>> :: ( binary() | integer() )
}).

-type column() :: #column{}.

-type user_string() :: binary().
-type password() :: binary().
-type hash() :: binary().

-type state() :: term().
