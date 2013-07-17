
-module(myproto_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, J), {I, {I, start_link, J}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link(Port::integer(), Handler::atom(), ParseQuery::boolean()) ->
	{ok, pid()} | {error, Reason::term()}.

start_link(Port, Handler, ParseQuery) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, Handler, ParseQuery]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port, Handler, Parser]) ->
    {ok, { {one_for_one, 5, 10}, [
    	?CHILD(my_acceptor, [Port, Handler, Parser])
    ]} }.

