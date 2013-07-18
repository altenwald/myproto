-module(gen_myproto).

-include("../include/myproto.hrl").

-callback check_pass(
	user_string(), 
	hash(),
	password()
) -> 
	{ok, password(), State::term()} | 
	{error, Reason::binary()} |
	{error, Code::integer(), Reason::binary()}.


-callback execute(
	Query :: request(),
	state()
) ->
	{reply, response(), state()} |
	{noreply, state()} |
	{stop, Reason::term(), state()}.

-callback terminate(
	Reason :: term(),
	state()
) ->
	ok.
