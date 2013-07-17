-module(gen_myproto).

-include("../include/myproto.hrl").

-callback check_pass(
	User::user_string(), 
	Hash::hash(),
	Password::password()
) -> 
	{ok, password(), State::term()} | 
	{error, Reason::binary()} |
	{error, Code::integer(), Reason::binary()}.


-callback execute(
	Query :: request(),
	State :: term()
) ->
	{#response{}, State::term()}.

-callback terminate(
	Reason :: term(),
	State :: term()
) ->
	ok.
