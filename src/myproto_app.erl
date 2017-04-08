-module(myproto_app).
-author('Manuel Rubio <manuel@altenwald.com>').

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, J), {I, {I, start_link, J}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Acceptor = case application:get_env(myproto, port) of
        {ok, Port} ->
            {ok, Handler} = application:get_env(myproto, handler),
            ParseQuery = application:get_env(myproto, parse_query, false),
            [?CHILD(my_acceptor, [Port, Handler, ParseQuery])];
        undefined ->
            []
    end,
    {ok, { {one_for_one, 5, 10}, Acceptor} }.
