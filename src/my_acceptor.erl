-module(my_acceptor).
-author('bombadil@bosqueviejo.net').
 
-behaviour(gen_server).
 
-define(SERVER, ?MODULE).
 
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
 
-record(state, {
    lsocket :: get_tcp:socket(), 
    id      :: integer(),         %% counter connection ID
    handler :: atom()             %% handler for queries/auth
}).

start_link(Port, Handler) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port, Handler], []).
 
init([Port, Handler]) ->
    Opts = [binary, {packet, 0}, {active, true}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opts) of
        {ok, LSocket} ->
            {ok, #state{lsocket=LSocket, id=1, handler=Handler}, 0};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
 
handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(timeout, State=#state{lsocket=LSocket, id=Id, handler=Handler}) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    Res = my_request:start(Socket, Id, Handler),
    lager:info("Incoming connection: ~p~n", [Res]),
    {noreply, State#state{id=(Id+1) rem 16#100000000}, 0};
 
handle_info(Info, State) ->
    lager:error("unknown message: ~p~n", [Info]),
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
