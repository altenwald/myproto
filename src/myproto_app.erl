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
    myproto_sup:start_link(Port, Handler).

stop(_State) ->
    ok.
