-module(my_request).
-author('Manuel Rubio <manuel@altenwald.com>').

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-include("myproto.hrl").

-export([
    start/4,
    check_clean_pass/2,
    check_sha1_pass/2,
    sha1_hex/1,
    to_hex/1,

    % FSM callbacks
    callback_mode/0,
    init/1,
    handle_event/4,
    terminate/3
]).

-record(state, {
    socket  :: gen_tcp:socket(),       %% TCP connection
    id      :: integer(),              %% connection id
    hash    :: binary(),               %% hash for auth
    handler :: atom(),                 %% Handler for auth/queries
    msg = <<>> :: binary(),            %% Received sql query when partial
    packet = <<>> :: binary(),         %% Received packet
    parse_query = false :: boolean(),  %% parse the received string or not
    handler_state
}).

%% API

-spec start(Socket :: gen_tcp:socket(),
            Id :: pos_integer(),
            Handler :: atom(),
            ParseQuery :: boolean()) -> {ok, pid()}.

start(Socket, Id, Handler, ParseQuery) ->
    {ok, Pid} = gen_statem:start(?MODULE, [Socket, Id, Handler, ParseQuery], []),
    gen_tcp:controlling_process(Socket, Pid),
    inet:setopts(Socket, [{active, true}]),
    {ok, Pid}.

-spec sha1_hex(Data :: binary()) -> binary().

sha1_hex(Data) ->
    to_hex(crypto:hash(sha, Data)).

-spec to_hex(Hash :: binary() | undefined) -> binary().

to_hex(<<>>) ->
    <<"undefined">>;
to_hex(undefined) ->
    <<"undefined">>;
to_hex(<<X:160/big-unsigned-integer>>) ->
    list_to_binary(io_lib:format("~40.16.0b", [X])).

-spec check_sha1_pass(Pass::binary(), Salt::binary()) -> binary().

check_sha1_pass(Stage, Salt) ->
    Res = crypto:hash_final(
        crypto:hash_update(
            crypto:hash_update(crypto:hash_init(sha), Salt),
            crypto:hash(sha, Stage)
        )
    ),
    crypto:exor(Stage, Res).

-spec check_clean_pass(Pass::binary(), Salt::binary()) -> binary().

check_clean_pass(Pass, Salt) ->
    check_sha1_pass(crypto:hash(sha, Pass), Salt).

%% callbacks

init([Socket, Id, Handler, ParseQuery]) ->
    Hash = list_to_binary(
        lists:map(fun
            (0) -> 1;
            (X) -> X
        end, binary_to_list(
            crypto:strong_rand_bytes(20)
        ))
    ),
    Hello = #response{
        id = Id,
        status = ?STATUS_HELLO,
        info = Hash
    },
    ok = my_response:send_or_reply(Hello, Socket),
    {ok, auth, #state{
        socket = Socket,
        id = Id,
        hash = Hash,
        handler = Handler,
        parse_query = ParseQuery}}.

callback_mode() ->
    handle_event_function.

handle_event(internal, {_Msg, {ok, #request{info = #user{password = Password} = User}, <<>>}},
             auth, #state{hash = Hash, socket = Socket, handler = Handler} = StateData) ->
    ?DEBUG("Hash=~p; Pass=~p~n", [to_hex(Hash), to_hex(Password)]),
    case Handler:check_pass(User#user{server_hash = Hash}) of
        {ok, Password, HandlerState} ->
            Response = #response{status = ?STATUS_OK,
                                 status_flags = ?SERVER_STATUS_AUTOCOMMIT,
                                 id = 2},
            ok = my_response:send_or_reply(Response, Socket),
            {next_state, normal, StateData#state{handler_state = HandlerState}};
        {error, Reason} ->
            Response = #response{status = ?STATUS_ERR,
                                 error_code = 1047,
                                 info = Reason,
                                 id = 2},
            ok = my_response:send_or_reply(Response, Socket),
            gen_tcp:close(Socket),
            {stop, normal, StateData};
        {error, Code, Reason} ->
            Response = #response{status = ?STATUS_ERR,
                                 error_code = Code,
                                 info = Reason,
                                 id = 2},
            ok = my_response:send_or_reply(Response, Socket),
            gen_tcp:close(Socket),
            {stop, normal, StateData};
        {error, Code, SQLState, Reason} ->
            Response = #response{status = ?STATUS_ERR,
                                 error_code = Code,
                                 error_info = SQLState,
                                 info = Reason,
                                 id = 2},
            ok = my_response:send_or_reply(Response, Socket),
            gen_tcp:close(Socket),
            {stop, normal, StateData}
    end;

handle_event(internal, {_Msg, {ok, #request{id = Id, info = Info, command = Command} = Request, <<>>}},
             normal, #state{socket = Socket,
                            handler = Handler,
                            packet = Packet,
                            handler_state = HandlerState} = StateData) ->
    ?DEBUG("Received: ~p~n", [Request]),
    FullPacket = <<Packet/binary, Info/binary>>,
    ParsedRequest = case StateData#state.parse_query andalso
                 Command =:= ?COM_QUERY of
        false ->
            Request#request{info = FullPacket};
        true ->
            case mysql_parser:parse(FullPacket) of
                {fail,Expected} ->
                    ?ERROR_MSG("SQL invalid: ~p~n", [Expected]),
                    Request#request{info = FullPacket};
                {_, Extra, Where} ->
                    ?ERROR_MSG("SQL error: ~p ~p~n", [Extra, Where]),
                    Request#request{info = FullPacket};
                Parsed ->
                    Request#request{info = Parsed}
            end
    end,
    NewStateData = StateData#state{packet = <<>>},
    case Handler:execute(ParsedRequest, HandlerState) of
        {noreply, NewHandlerState} ->
            {next_state,
             normal,
             NewStateData#state{handler_state = NewHandlerState}};
        {reply, #response{} = Response, NewHandlerState} ->
            ok = my_response:send_or_reply(Response#response{id = Id + 1},
                                           Socket),
            {next_state,
             normal,
             NewStateData#state{handler_state = NewHandlerState}};
        {reply, default, NewHandlerState} ->
            {reply, Response, ModHandlerState} =
                my_response:default_reply(ParsedRequest, Handler,
                                          NewHandlerState),
            error_logger:info_msg("default reply: ~p~n", [Response]),
            ok = my_response:send_or_reply(Response#response{id = Id + 1},
                                           Socket),
            {next_state,
             normal,
             NewStateData#state{handler_state = ModHandlerState}};
        {stop, Reason, NewHandlerState} ->
            {stop,
             Reason,
             NewStateData#state{handler_state = NewHandlerState}}
    end;

handle_event(internal, {_Msg, {ok, #request{continue = true, info = Info} = Request, <<>>}},
             _StateName, StateData) ->
    ?DEBUG("Received (partial): ~p~n", [Request]),
    Packet = StateData#state.packet,
    {next_state, normal,
     StateData#state{packet = <<Packet/binary, Info/binary>>}};

handle_event(internal, {Msg, {more, _NumBytes}}, _StateName, StateData) ->
    ?DEBUG("Received (partial): bytes remaining = ~w~n", [_NumBytes]),
    {next_state, normal, StateData#state{msg = Msg}};

handle_event(info, {tcp, _Port, Msg}, auth, #state{msg = PrevMsg}) ->
    Msg2 = <<PrevMsg/binary, Msg/binary>>,
    DecMsg2 = my_packet:decode_auth(Msg2),
    {keep_state_and_data, [{next_event, internal, {Msg2, DecMsg2}}]};

handle_event(info, {tcp, _Port, Msg}, normal, #state{msg = PrevMsg}) ->
    Msg2 = <<PrevMsg/binary, Msg/binary>>,
    DecMsg2 = my_packet:decode(Msg2),
    {keep_state_and_data, [{next_event, internal, {Msg2, DecMsg2}}]};

handle_event(info, {tcp_closed, _Socket}, _StateName, #state{id = Id} = StateData) ->
    ?INFO_MSG("Connection ID#~w closed~n", [Id]),
    {stop, normal, StateData};

handle_event(info, Info, _StateName, StateData = #state{socket = Socket}) ->
    ?ERROR_MSG("unknown message: ~p~n", [Info]),
    gen_tcp:close(Socket),
    {stop, normal, StateData}.

terminate(Reason, _StateName, #state{handler = Handler,
                                     handler_state = HandlerState}) ->
    Handler:terminate(Reason, HandlerState),
    ok.
