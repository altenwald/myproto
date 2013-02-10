
-module(myproto_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, J), {I, {I, start_link, J}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Port, Handler) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, Handler]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port, Handler]) ->
    {ok, { {one_for_one, 5, 10}, [
    	?CHILD(my_acceptor, [Port, Handler])
    ]} }.

