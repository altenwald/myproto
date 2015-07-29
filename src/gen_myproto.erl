-module(gen_myproto).

-include("../include/myproto.hrl").

-callback check_pass(
	#user{}
) -> 
	{ok, password(), State::term()} | 
	{error, Reason::binary()} |
	{error, Code::integer(), Reason::binary()}.


-type metadata() ::
	{connect_db, binary()} |
	databases |
	tables |
	version.


-callback metadata(metadata(), state()) ->
	{reply, Value::any(), state()} |
	{error, Reason::any(), state()} |
	{noreply, state()}.


-callback execute(
	Query :: request(),
	state()
) ->
	{reply, default, state()} |
	{reply, response(), state()} |
	{noreply, state()} |
	{stop, Reason::term(), state()}.

-callback terminate(
	Reason :: term(),
	state()
) ->
	ok.
