-module(gen_myproto).
-author('Manuel Rubio <manuel@altenwald.com>').

-include("myproto.hrl").

-callback check_pass(user_string()) ->
    {ok, password(), state()} |
    {error, reason()} |
    {error, code(), reason()}.


-type metadata() ::
    {connect_db, binary()} |
    databases |
    tables |
    version.


-callback metadata(metadata(), state()) ->
    {reply, Value::any(), state()} |
    {error, reason(), state()} |
    {noreply, state()}.


-callback execute(Query :: request(), state()) ->
    {reply, default, state()} |
    {reply, response(), state()} |
    {noreply, state()} |
    {stop, reason(), state()}.


-callback terminate(reason(), state()) -> ok.
