Configure and running
=====================

If you want configure the port, the handler and the server sign:

```erlang
  {myproto, [
    {handler, my_dummy_handler},
    {parse_query, true},
    {server_sign, <<"5.5-myproto">>},
    {default_storage_engine, <<"myproto">>},
    {port, 3306}
  ]}.
```

A dummy test
------------

This is a little tutorial to show you the use of this library. As a little introduction, you can create your environment as follow:

```shell
rebar3 new release name=mydummy
cd mydummy
```

Add the dependency to the `rebar.config` file:

```erlang
{deps, [
  {myproto, {git, "git://github.com/altenwald/myproto.git", {branch, master}}}
]}.
```

Now, you can create the `apps/mydummy/src/my_dummy_handler.erl` module:

```erlang
-module(my_dummy_handler).
-behaviour(gen_myproto).

-include_lib("myproto/include/myproto.hrl").

-export([check_pass/1, metadata/2, execute/2, terminate/2]).

-record(my, {
    db
}).

check_pass(#user{name = Name, server_hash = Hash, password = Password}) ->
    case my_request:check_clean_pass(Name, Hash) of
        Password -> {ok, Password, #my{}};
        _ -> {error, <<"Password incorrect!">>}
    end.


metadata(version, State) ->
    {reply, <<"my cool server 1.0">>, State};

metadata({connect_db, Database}, State) ->
    {noreply, State#my{db = Database}};

metadata(databases, State) ->
    {reply, [<<"storage">>], State};

metadata(tables, #my{db = <<"storage">>} = State) ->
    {reply, {<<"storage">>, [<<"channels">>, <<"users">>]}, State};

metadata({fields, <<"channels">> = Table}, #my{db = <<"storage">> = DB} = State) ->
    {reply, {DB, Table, [{name,string},{users_count,integer},{started_at,integer}]}, State};

metadata(_, #my{} = State) ->
    {noreply, State}.


execute(#request{info =
            #select{params=[#variable{name = <<"version_comment">>}]
        }}, State) ->
    Info = {
        [#column{name = <<"@@version_comment">>, type=?TYPE_VARCHAR, length=20}],
        [[<<"myproto 0.1">>]]
    },
    {reply, #response{status=?STATUS_OK, info=Info}, State};

execute(#request{command = ?COM_QUIT}, State) ->
    {stop, normal, State};

execute(#request{info = #select{}} = Request, State) ->
    Info = {
        [
            #column{name = <<"Info">>, type=?TYPE_VARCHAR, length=20},
            #column{name = <<"Info2">>, type=?TYPE_VARCHAR, length=20}
        ],
        [
            [<<"Not implemented!">>, <<"Yet">>],
            [<<"Testing MultiColumn!">>, <<"Still">>]
        ]
    },
    {reply, #response{status=?STATUS_OK, info=Info}, State};

execute(#request{} = Request, State) ->
    {reply, default, State}. % Return default reply if you don't know answer on this request


terminate(_Reason, _State) ->
    ok.
```

Please, mention that dummy module must implement metadata callback, because usually mysql clients ask a lot of information on start about databases, tables and fields in them.

For example, mysql has three different protocols for querying table structure. myproto hides this stuff from you in a very simple and convenient way.

Now time to build:

```shell
rebar3 compile
```

And time to launch:

```shell
rebar3 shell
```

Now, to test, in another terminal or shell:

```shell
mysql -uroot -proot -h127.0.0.1
```

You can use some SQL statements as:

```sql
SELECT @@version_comment;
SELECT * FROM sometable;
```

Done :-)
