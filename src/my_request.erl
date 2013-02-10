-module(my_request).
-author('bombadil@bosqueviejo.net').
 
-behaviour(gen_fsm).
 
-define(SERVER, ?MODULE).

-include("../include/myproto.hrl").
 
-export([start/2, check_pass/2, sha1_hex/1, to_hex/1]).
-export([init/1, handle_sync_event/4, handle_event/3, handle_info/3,
         terminate/3, code_change/4]).
 
-record(state, {socket, id, hash}).
 

%% API

start(Socket, Id) ->
    {ok, Pid} = gen_fsm:start(?MODULE, [Socket, Id], []),
    gen_tcp:controlling_process(Socket, Pid),
    inet:setopts(Socket, [{active, true}]),
    {ok, Pid}.

sha1_hex(Data) ->
    to_hex(crypto:sha(Data)).

to_hex(<<X:160/big-unsigned-integer>>) ->
    list_to_binary(io_lib:format("~40.16.0b", [X])).

check_pass(Pass, Salt) ->
    Stage1 = crypto:sha(Pass),
    Stage2 = crypto:sha(Stage1),
    Res = crypto:sha_final(
        crypto:sha_update(
            crypto:sha_update(crypto:sha_init(), Salt),
            Stage2
        )
    ),
    crypto:exor(Stage1, Res).

%% callbacks

init([Socket, Id]) ->
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
    {ok, auth, #state{socket=Socket, id=Id, hash=Hash}}.

handle_info({tcp,_Port, Info}, auth, StateData=#state{hash=Hash,socket=Socket}) ->
    #request{info=#user{
        name=User, password=Password
    }} = my_packet:decode_auth(Info),
    error_logger:info_msg("Hash=~p~nPass=~p~n", [to_hex(Hash),to_hex(Password)]),
    case check_pass(User, Hash) of
        Password ->
            Response = #response{
                status = ?STATUS_OK,
                status_flags = ?SERVER_STATUS_AUTOCOMMIT,
                id = 2
            },
            gen_tcp:send(Socket, my_packet:encode(Response)), 
            {next_state, normal, StateData};
        _ ->
            Response = #response{
                status = ?STATUS_ERR,
                error_code = 2003,
                info = <<"Can't connect to DataHUB, incorrect password.">>,
                id = 2
            },
            gen_tcp:send(Socket, my_packet:encode(Response)),
            gen_tcp:close(Socket),
            {stop, normal, StateData}
    end;

handle_info({tcp,_Port,Info}, normal, #state{socket=Socket}=StateData) ->
    Request = my_packet:decode(Info),
    error_logger:info_msg("Received: ~p~n", [Request]),
    gen_tcp:send(Socket, my_packet:encode(
        [#column{name = <<"@@version_comment">>, type=?TYPE_VARCHAR, length=20}],
        [<<"DataHUB 0.1">>]
    )),
    {next_state, normal, StateData};

handle_info(Info, _StateName, StateData=#state{socket=Socket}) ->
    error_logger:info_msg("Unknown message: ~p~n", [Info]),
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


%% Internal functions
