-module(my_acceptor).
-author('Manuel Rubio <manuel@altenwald.com>').

-behaviour(gen_server).

-include("myproto.hrl").

-export([start_link/3, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TIMEOUT_ACCEPTOR, 1000). % ms

-record(state, {
    lsocket :: get_tcp:socket(),
    id = 1  :: integer(),         %% counter connection ID
    handler :: atom(),            %% handler for queries/auth
    parse_query :: boolean()      %% if the query will be parsed or not
}).


start_link(Port, Handler, ParseQuery) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [Port, Handler, ParseQuery], []).


stop() ->
    gen_server:call(?MODULE, stop).


init([Port, Handler, ParseQuery]) ->
    Opts = [binary, {packet, 0}, {active, true}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opts) of
        {ok, LSocket} ->
            ?DEBUG("Listening in port ~p~n", [inet:port(LSocket)]),
            {ok, #state{lsocket=LSocket,
                        handler=Handler,
                        parse_query=ParseQuery}, 0};
        {error, Reason} ->
            {stop, Reason}
    end.


handle_call(port, _From, #state{lsocket = LSocket} = State) ->
    {reply, inet:port(LSocket), State, 0};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(timeout, State = #state{lsocket = LSocket,
                                    id = Id,
                                    handler = Handler,
                                    parse_query = ParseQuery}) ->
    case gen_tcp:accept(LSocket, ?TIMEOUT_ACCEPTOR) of
        {ok, Socket} ->
            Res = my_request:start(Socket, Id, Handler, ParseQuery),
            ?INFO_MSG("Incoming connection: ~p~n", [Res]),
            {noreply, State#state{id=(Id+1) rem 16#100000000}, 0};
        {error, closed} ->
            {stop, normal, State};
        {error, timeout} ->
            {noreply, State, 0}
    end.


terminate(_Reason, State) ->
    ?INFO_MSG("closing ~p~n", [State#state.id]),
    ok = gen_tcp:close(State#state.lsocket),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
