-module(my_acceptor).
-author('bombadil@bosqueviejo.net').
 
-behaviour(gen_server).
 
-define(SERVER, ?MODULE).
 
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
 
-record(state, {lsocket, id}).
 
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
init([]) ->
	random:seed(now()),
    Opts = [binary, {packet, 0}, {active, true}],
    case gen_tcp:listen(5000, Opts) of
        {ok, LSocket} ->
            {ok, #state{lsocket=LSocket, id=1}, 0};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
 
handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(timeout, State=#state{lsocket=LSocket, id=Id}) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    Res = my_request:start(Socket, Id),
    error_logger:info_msg("~p~n", [Res]),
    {noreply, State#state{id=Id+1}, 0};
 
handle_info(Info, State) ->
	error_logger:info_msg("Unknown message: ~p~n", [Info]),
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
