Overview
========

[![Build Status](https://api.travis-ci.org/altenwald/myproto.png)](https://travis-ci.org/altenwald/myproto)

MySQL Server Protocol in Erlang. This project let you implement the MySQL protocol for your server. Throught a MySQL connection you could send queries or fake a MySQL connection to do a proxy or whatever else.

Usage
-----

If you want to use, only add this in rebar.config:

```erlang
  {deps, [
      {myproto, ".*", {git, "git://github.com/altenwald/myproto.git", master}}
  ]}.
```

Configuration
-------------

If you want configure the port, the handler and the server sign:

```erlang
  {myproto, [
    {handler, my_dummy_handler},
    {parse_query, true},
    {server_sign, <<"5.5-myproto">>},
    {port, 3306}
  ]}.
```

A dummy test
------------

This is a little tutorial to show you the use of this library. As a little introduction, you can create your environment as follow:

```shell
mkdir -p mydummy/apps/mydummy
cd mydummy/apps/mydummy
rebar create template=simpleapp appid=mydummy
cd ../..

mkdir rel
cd rel
rebar create template=simplenode nodeid=mydummy
cd ..
```

Create the file rebar.config as follow:

```erlang
{sub_dirs, ["apps/*", "rel"]}.
{deps, [
  {myproto, ".*", {git, "git://github.com/altenwald/myproto.git", master}}
]}.
```

Modify the file `rel/files/sys.config` as:

```erlang
[
 {myproto, [
    {handler, mydummy},
    {parse_query, true},
    {server_sign, <<"5.5-myproto">>},
    {port, 3306}
 ]},
 
 %% SASL config
 {sasl, [
    {sasl_error_logger, {file, "log/sasl-error.log"}},
    {errlog_type, error},
    {error_logger_mf_dir, "log/sasl"},      % Log directory
    {error_logger_mf_maxbytes, 10485760},   % 10 MB max file size
    {error_logger_mf_maxfiles, 5}           % 5 files max
 ]}
].
```

Now, you can create the `apps/mydummy/src/mydummy.erl` module:

```erlang
-module(mydummy).
-behaviour(gen_myproto).

-include_lib("myproto/include/myproto.hrl").

-export([check_pass/3, execute/2, terminate/2]).

-record(my, {
    db
}).

check_pass(User, Hash, Password) ->
    case my_request:check_clean_pass(User, Hash) of
        HashedPassword -> {ok, HashedPassword, #my{}};
        _ -> {error, <<"Password incorrect!">>}
    end.

metadata(version, State) ->
    {reply, <<"my cool server 1.0">>, State};

metadata({connect_db, Database}, State) ->
    {noreply, State#my{db = Database}};

metadata(databases, State) ->
    {reply, [<<"comet">>], State};

metadata(tables, #db{db = <<"storage">>} = State) ->
    {reply, {<<"storage">>, [<<"channels">>, <<"users">>]}, State};

metadata({fields, <<"channels">> = Table}, #db{db = <<"storage">> = DB} = State) ->
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
    lager:info("Exiting~n", []),
    {stop, normal, State};
execute(#request{info = #select{}} = Request, State) ->
    lager:info("Request: ~p~n", [Request]),
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
    lager:info("Unknown request: ~p", [Request]),
    {reply, default, State}. % Return default reply if you don't know answer on this request

terminate(_Reason, _State) ->
    ok.
```


Please, mention that dummy module must implement metadata callback, because usually mysql clients
ask a lot of information on start about databases, tables and fields in them.

For example, mysql has three different protocols for querying table structure. myproto hides this stuff
from you in a very simple and convenient way.



Add this to `rel/reltool.config`:

```erlang
{sys,
    {lib_dirs, ["../apps", "../deps"]},
    {erts, [{mod_cond, derived}, {app_file, strip}]},
    {app_file, strip},
    {rel, "mydummy", "1", [
        kernel,
        stdlib,
        sasl,
        inets,
        lager,
        myproto,
        mydummy
    ]},
    ...
```

Now time to build:

```shell
rebar get-deps compile generate
```

And time to launch:

```shell
rel/mydummy/bin/mydummy console
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


[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/altenwald/myproto/trend.png)](https://bitdeli.com/free "Bitdeli Badge")

