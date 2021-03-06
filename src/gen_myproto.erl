-module(gen_myproto).
-author('Manuel Rubio <manuel@altenwald.com>').

-include("myproto.hrl").

-callback check_pass(user_string()) -> check_pass_resp().

-type check_pass_resp() ::
    {ok, password(), state()} |
    {error, reason()} |
    {error, code(), reason()} |
    {error, code(), sqlstate(), reason()}.


-type metadata() ::
    {connect_db, binary()} |
    databases |
    tables |
    version.


-callback metadata(metadata(), state()) -> metadata_resp().

-type metadata_resp() ::
    {reply, Value::any(), state()} |
    {error, reason(), state()} |
    {noreply, state()}.


-callback execute(Query :: request(), state()) -> execute_resp().

-type execute_resp() ::
    {reply, default, state()} |
    {reply, response(), state()} |
    {noreply, state()} |
    {stop, reason(), state()}.


-callback terminate(reason(), state()) -> ok.
