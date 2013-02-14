-module(my_request).
-author('bombadil@bosqueviejo.net').
 
-behaviour(gen_fsm).
 
-define(SERVER, ?MODULE).

-include("../include/myproto.hrl").
 
-export([start/3, check_clean_pass/2, check_sha1_pass/2, sha1_hex/1, to_hex/1]).
-export([init/1, handle_sync_event/4, handle_event/3, handle_info/3,
         terminate/3, code_change/4]).
 
-record(state, {
    socket  :: gen_tcp:socket(), %% TCP connection
    id      :: integer(),        %% connection id
    hash    :: binary(),         %% hash for auth
    handler :: atom(),           %% Handler for auth/queries
    packet = <<"">> :: binary()  %% Received packet
}).

%% API

-spec start(Socket :: gen_tcp:socket(), Id :: integer(), Handler :: atom()) -> {ok, pid()}.

start(Socket, Id, Handler) ->
    {ok, Pid} = gen_fsm:start(?MODULE, [Socket, Id, Handler], []),
    gen_tcp:controlling_process(Socket, Pid),
    inet:setopts(Socket, [{active, true}]),
    {ok, Pid}.

-spec sha1_hex(Data :: binary()) -> binary().

sha1_hex(Data) ->
    to_hex(crypto:sha(Data)).

-spec to_hex(Hash :: binary()) -> binary().

to_hex(<<X:160/big-unsigned-integer>>) ->
    list_to_binary(io_lib:format("~40.16.0b", [X])).

-spec check_sha1_pass(Pass::binary(), Salt::binary()) -> binary().

check_sha1_pass(Stage1, Salt) ->
    Stage2 = crypto:sha(Stage1),
    Stage2 = crypto:sha(Stage1),
    Res = crypto:sha_final(
        crypto:sha_update(
            crypto:sha_update(crypto:sha_init(), Salt),
            Stage2
        )
    ),
    crypto:exor(Stage1, Res).

-spec check_clean_pass(Pass::binary(), Salt::binary()) -> binary().

check_clean_pass(Pass, Salt) ->
    Stage1 = crypto:sha(Pass),
    check_sha1_pass(Stage1, Salt).

%% callbacks

init([Socket, Id, Handler]) ->
    Hash = list_to_binary(
        lists:map(fun
            (0) -> 1; 
            (X) -> X 
        end, binary_to_list(
            crypto:rand_bytes(20)
        ))
    ),
    Hello = #response{
        id=Id, 
        status=?STATUS_HELLO, 
        info=Hash
    },
    gen_tcp:send(Socket, my_packet:encode(Hello)),
    {ok, auth, #state{socket=Socket, id=Id, hash=Hash, handler=Handler}}.

handle_info({tcp,_Port, Info}, auth, StateData=#state{hash=Hash,socket=Socket,handler=Handler}) ->
    #request{info=#user{
        name=User, password=Password
    }} = my_packet:decode_auth(Info),
    lager:debug("Hash=~p; Pass=~p~n", [to_hex(Hash),to_hex(Password)]),
    case Handler:check_pass(User, Hash, Password) of
        Password ->
            Response = #response{
                status = ?STATUS_OK,
                status_flags = ?SERVER_STATUS_AUTOCOMMIT,
                id = 2
            },
            gen_tcp:send(Socket, my_packet:encode(Response)), 
            {next_state, normal, StateData};
        {error, Reason} ->
            Response = #response{
                status = ?STATUS_ERR,
                error_code = 2003,
                info = Reason,
                id = 2
            },
            gen_tcp:send(Socket, my_packet:encode(Response)),
            gen_tcp:close(Socket),
            {stop, normal, StateData}
    end;

handle_info({tcp,_Port,Msg}, normal, #state{socket=Socket,handler=Handler,packet=Packet}=StateData) ->
    case my_packet:decode(Msg) of
        #request{continue=true, info=Info}=Request ->
            lager:info("Received (partial): ~p~n", [Request]),
            {next_state, normal, StateData#state{packet = <<Packet/binary, Info/binary>>}};
        #request{continue=false, info=Info}=Request ->
            lager:info("Received: ~p~n", [Request]),
            gen_tcp:send(Socket, my_packet:encode(
                Handler:execute(Request#request{info = <<Packet/binary, Info/binary>>})
            )),
            {next_state, normal, StateData#state{packet = <<"">>}}
    end;

handle_info({tcp_closed, _Socket}, _StateName, #state{id=Id}=StateData) ->
    lager:info("Connection ID#~w closed~n", [Id]),
    {stop, normal, StateData};

handle_info(Info, _StateName, StateData=#state{socket=Socket}) ->
    lager:error("unknown message: ~p~n", [Info]),
    gen_tcp:close(Socket),
    {stop, normal, StateData}.
 
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.
 
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
