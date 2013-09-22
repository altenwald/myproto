-module(my_ranch_worker).

-export([start_server/4, stop_server/1, start_link/4, init_server/4]).

-export([handle_call/3, handle_info/2, terminate/2]).
-include("../include/myproto.hrl").


start_server(Port, Name, Handler, Args) ->
  application:start(ranch),
  ranch:start_listener(Name, 10, ranch_tcp, [{port, Port},{backlog,4096},{max_connections,32768}], ?MODULE, [Handler, Args]).

stop_server(Name) ->
  ranch:stop_listener(Name).


start_link(ListenerPid, Socket, _Transport, [Handler, Args]) ->
  proc_lib:start_link(?MODULE, init_server, [ListenerPid, Socket, Handler, Args]).



-record(server, {
  handler,
  state,
  socket,
  my
}).

init_server(ListenerPid, Socket, Handler, _Args) ->
  proc_lib:init_ack({ok, self()}),
  ranch:accept_ack(ListenerPid),

  My0 = my_protocol:init([{socket, Socket}]),
  {ok, My1} = my_protocol:hello(42, My0),
  {ok, #request{info = #user{name=User, password=Password, server_hash = Hash}}, My2} = my_protocol:next_packet(My1),
  case Handler:check_pass(User, Hash, Password) of
    {ok, Password, HandlerState} ->
      {ok, My3} = my_protocol:ok(My2),
      inet:setopts(Socket, [{active,once}]),
      State = #server{handler = Handler, state = HandlerState, socket = Socket, my = My3},
      gen_server:enter_loop(?MODULE, [], State);
    {error, Reason} ->
      my_protocol:error(Reason, My2);
    {error, Code, Reason} ->
      my_protocol:error(Code, Reason, My2)
  end.

handle_call(Call, _From, #server{} = Server) ->
  {stop, {unknown_call,Call}, Server}.


handle_info({tcp, Socket, Bin}, #server{my = My} = Server) ->
  My1 = my_protocol:buffer_bytes(Bin, My),
  inet:setopts(Socket, [{active, once}]),
  try handle_packets(Server#server{my = My1})
  catch
    Class:Error -> 
      lager:info("~p:~p during handling mysql:\n~p\n", [Class, Error, erlang:get_stacktrace()]),
      {stop, normal, Server}
  end;

handle_info({tcp_closed, _Socket}, #server{} = Server) ->
  {stop, normal, Server};

handle_info({tcp_error, _, _}, #server{} = Server) ->
  {stop, normal, Server}.


terminate(_,_) -> ok.



handle_packets(#server{my = My, handler = Handler, state = HandlerState} = Server) ->
  case my_protocol:decode(My) of
    {ok, Query, My1} ->
      case Handler:execute(Query, HandlerState) of
        {noreply, HandlerState1} ->
          handle_packets(Server#server{my = My1, state = HandlerState1});
        {reply, Response, HandlerState1} ->
          {ok, My2} = my_protocol:send_or_reply(Response, My1),
          handle_packets(Server#server{my = My2, state = HandlerState1});
        {stop, Reason, HandlerState1} ->
          {stop, Reason, Server#server{my = My1, state = HandlerState1}}
      end;
    {more, My1} ->
      {noreply, Server#server{my = My1}}
  end.







