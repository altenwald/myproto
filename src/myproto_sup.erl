
-module(myproto_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, J), {I, {I, start_link, J}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() ->
	{ok, pid()} | {error, Reason::term()}.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Acceptor = case application:get_env(myproto, port) of
    {ok, Port} ->
      {ok, Handler} = application:get_env(myproto, handler),
      ParseQuery = case application:get_env(myproto, parse_query) of 
        {ok, PQ} ->  PQ;
        _ -> false
      end,
      [?CHILD(my_acceptor, [Port, Handler, ParseQuery])];
    undefined ->
      []
  end,

  {ok, { {one_for_one, 5, 10}, Acceptor} }.

