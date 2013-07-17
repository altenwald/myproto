-module(myproto_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	{ok, Port} = application:get_env(myproto, port),
	{ok, Handler} = application:get_env(myproto, handler),
	ParseQuery = case application:get_env(myproto, parse_query) of 
		{ok, PQ} ->  PQ;
		_ -> false
	end,
    myproto_sup:start_link(Port, Handler, ParseQuery).

stop(_State) ->
    ok.
