-module(gen_myproto).

-include("../include/myproto.hrl").

-callback check_pass(
	User::binary(), 
	Hash::binary(),
	Password::binary()
) -> 
	binary() | 
	{error, Reason::binary()}.


-callback execute(
	Query :: #request{}
) ->
	{list(#column{}), list(binary())} | #response{}.
