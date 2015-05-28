-module(sql_SUITE).

% -include("../include/sql.hrl").
-include("../include/myproto.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


all() ->
  [{group,parse}].


groups() ->
  [{parse, [parallel], [
    delete_simple,
    delete_where,
    insert_simple,
    insert_keys,
    insert_set,
    show,
    show_like,
    transaction,
    set,
    select_all,
    select_strings,
    select_simple,
    select_simple_multiparams,
    select_simple_subquery,
    select_from,
    select_from_subquery,
    select_where,
    select_function,
    select_groupby,
    select_orderby,
    select_limit,
    select_arithmetic,
    select_variable,

    server_select_simple,
    server_reject_password,
    server_very_long_query,
    update_simple,
    update_multiparams,
    update_where,
    long_query_2
  ]}].


init_per_suite(Config) -> application:ensure_all_started(ranch), Config.
end_per_suite(Config) -> Config.

transaction(_) ->
  'begin' = mysql_proto:parse("begin"),
  'commit' = mysql_proto:parse("commit"),
  'rollback' = mysql_proto:parse("rollback"),
  ok.



delete_simple(_) ->
  #delete{table=#table{name = <<"mitabla">>, alias = <<"mitabla">>}} = mysql_proto:parse("delete from mitabla").


delete_where(_) ->
  #delete{
        table=#table{name = <<"mitabla">>, alias = <<"mitabla">>},
        conditions=#condition{
            nexo=eq,
            op1=#key{name = <<"dato">>, alias = <<"dato">>},
            op2=#value{value = <<"this ain't a love song">>}
        }
  } = mysql_proto:parse("delete from mitabla where dato='this ain''t a love song'").



insert_simple(_) ->
  #insert{table = #table{name = <<"mitabla">>, alias = <<"mitabla">>}, values=[
    #value{value=1}, #value{value=2}, #value{value=3}
  ]} = mysql_proto:parse("insert into mitabla values (1,2,3)").


insert_keys(_) ->
  #insert{table = #table{name = <<"mitabla">>,
                 alias = <<"mitabla">>},
  values = [#set{key = <<"id">>,
                 value = #value{value = 1}},
            #set{key = <<"author">>,
                 value = #value{value = <<"bonjovi">>}},
            #set{key = <<"song">>,
                 value = #value{value = <<"these days">>}}]} =
  mysql_proto:parse("insert into mitabla(id,author,song) values(1,'bonjovi', 'these days')").


insert_set(_) ->
  A = mysql_proto:parse("insert into mitabla(id,author,song) values(1,'bonjovi', 'these days')"),
  B = mysql_proto:parse("insert into mitabla set id=1, author='bonjovi', song='these days'"),
  A = B.




show(_) ->
  #show{type=databases} = mysql_proto:parse("SHOW databases"),
  #show{type=variables} = mysql_proto:parse("SHOW variables"),
  #show{type=tables, full = true} = mysql_proto:parse("SHOW FULL tables"),
  #show{type=tables, full = false} = mysql_proto:parse("SHOW tables"),
  #show{type=fields,full=true,from= <<"streams">>} = mysql_proto:parse("SHOW FULL FIELDS FROM `streams`"),
  #show{type=fields,full=false,from= <<"streams">>} = mysql_proto:parse("SHOW FIELDS FROM `streams`"),
  #show{type=tables,full=false,from= {like,<<"streams">>}} = mysql_proto:parse("SHOW TABLES LIKE 'streams'"),
  #show{type=create_table,from= <<"streams">>} = mysql_proto:parse("SHOW CREATE TABLE `streams`"),
  #show{type=variables, conditions=#condition{
    nexo = eq,
    op1 = #key{name = <<"Variable_name">>},
    op2 = #value{value = <<"character_set_client">>}
  }} = mysql_proto:parse("SHOW VARIABLES WHERE Variable_name = 'character_set_client'"),

  #show{type=collation, conditions=#condition{
    nexo = eq,
    op1 = #key{name= <<"Charset">>},
    op2 = #value{value = <<"utf8">>}
  }} = mysql_proto:parse("show collation where Charset = 'utf8'"),
  ok.


show_like(_) ->
  #show{type=variables, conditions = {like, <<"sql_mode">>}} = mysql_proto:parse("SHOW VARIABLES LIKE 'sql_mode'"),
  ok.



set(_) ->
  #system_set{query=[{#variable{name = <<"a">>, scope = session},0}]} = mysql_proto:parse("SET a=0"),
  #system_set{query=[{#variable{name = <<"NAMES">>},<<"utf8">>}]} = mysql_proto:parse("SET NAMES 'utf8'"),

  #system_set{query=[
      {#variable{name = <<"SQL_AUTO_IS_NULL">>, scope = session},0},
      {#variable{name = <<"NAMES">>},<<"utf8">>},
      {#variable{name = <<"wait_timeout">>, scope = local}, 2147483}
  ]} = mysql_proto:parse("SET SQL_AUTO_IS_NULL=0, NAMES 'utf8', @@wait_timeout = 2147483"),
  ok.


select_variable(_) ->
  #select{params = [#variable{name = <<"max_allowed_packet">>, scope = local}]} = 
    mysql_proto:parse("SELECT @@max_allowed_packet"),
  #select{params = [#variable{name = <<"global.max_allowed_packet">>, scope = local}]} = 
    mysql_proto:parse("SELECT @@global.max_allowed_packet"),
  ok.



select_all(_) ->
  #select{params=[#all{}]} = mysql_proto:parse("select *"),
  #select{params=[#all{}]} = mysql_proto:parse("SELECT *"),
  #select{params=[#all{}]} = mysql_proto:parse(" Select    *   "),
  ok.

select_strings(_) ->
  #select{params = [#value{value = <<"hola'mundo">>}]} = mysql_proto:parse("select 'hola''mundo'").


select_simple(_) ->
    ?assertEqual(mysql_proto:parse("select 'hi' as message"),
        #select{params=[#value{name = <<"message">>,value = <<"hi">>}]}
    ),
    ?assertEqual(mysql_proto:parse("select 'hi'"),
        #select{params=[#value{value = <<"hi">>}]}
    ),
    ?assertEqual(mysql_proto:parse("select hi"),
        #select{params=[#key{alias = <<"hi">>,name = <<"hi">>}]}
    ),
    ?assertEqual(mysql_proto:parse("select hi as hello"),
        #select{params=[#key{alias = <<"hello">>,name = <<"hi">>}]}
    ),
    ?assertEqual(mysql_proto:parse("select a.hi"),
        #select{params=[#key{alias = <<"hi">>,name = <<"hi">>,table = <<"a">>}]}
    ),
    ?assertEqual(mysql_proto:parse("select aa.hi as hello"),
        #select{params=[#key{alias = <<"hello">>,name = <<"hi">>,table = <<"aa">>}]}
    ),
    ok.

select_simple_multiparams(_) ->
    ?assertEqual(mysql_proto:parse("select 'hi' as message, 1 as id"),
        #select{params=[#value{name = <<"message">>,value = <<"hi">>},#value{name = <<"id">>,value=1}]}
    ),
    ?assertEqual(mysql_proto:parse("select 'hi', 1"),
        #select{params=[#value{value = <<"hi">>},#value{value=1}]}
    ),
    ?assertEqual(mysql_proto:parse("select hi, message"),
        #select{params=[#key{alias = <<"hi">>,name = <<"hi">>},
        #key{alias = <<"message">>,name = <<"message">>}]}
    ),
    ?assertEqual(mysql_proto:parse("select hi as hello, message as msg"),
        #select{params=[#key{alias = <<"hello">>,name = <<"hi">>},
        #key{alias = <<"msg">>,name = <<"message">>}]}
    ),
    ?assertEqual(mysql_proto:parse("select a.hi, a.message"),
        #select{params=[#key{alias = <<"hi">>,name = <<"hi">>,table = <<"a">>},
        #key{alias = <<"message">>,name = <<"message">>,table = <<"a">>}]}
    ),
    ?assertEqual(mysql_proto:parse("select aa.hi as hello, aa.message as msg"),
        #select{params=[#key{alias = <<"hello">>,name = <<"hi">>,table = <<"aa">>},
        #key{alias = <<"msg">>,name = <<"message">>,table = <<"aa">>}]}
    ),
    ?assertEqual(mysql_proto:parse("select a.*, b.*"),
        #select{params=[#all{table = <<"a">>}, #all{table = <<"b">>}]}
    ),
    ?assertEqual(mysql_proto:parse("select *, a.*, b.*"),
        #select{params=[#all{}, #all{table = <<"a">>}, #all{table = <<"b">>}]}
    ),
    ok.

select_simple_subquery(_) ->
    ?assertEqual(mysql_proto:parse("select (select *)"),
        #select{params=[#subquery{subquery=#select{params=[#all{}]}}]}
    ),
    ?assertEqual(mysql_proto:parse("select (select *), id"),
        #select{params=[#subquery{subquery=#select{params=[#all{}]}},
        #key{alias = <<"id">>,name = <<"id">>}]}
    ),
    ?assertEqual(mysql_proto:parse("select (select uno) as uno, dos"),
        #select{params=[#subquery{name = <<"uno">>,
                   subquery=#select{params=[#key{alias = <<"uno">>,name = <<"uno">>}]}},
        #key{alias = <<"dos">>,name = <<"dos">>}]}
    ),
    ok.

select_from(_) ->
    ?assertEqual(mysql_proto:parse("select * from data"),
        #select{params=[#all{}],tables=[#table{name = <<"data">>,alias = <<"data">>}]}
    ),
    ?assertEqual(mysql_proto:parse("select uno, dos from data, data2"),
        #select{params = [#key{alias = <<"uno">>,name = <<"uno">>},
                  #key{alias = <<"dos">>,name = <<"dos">>}],
        tables = [#table{name = <<"data">>,alias = <<"data">>},
                  #table{name = <<"data2">>,alias = <<"data2">>}]}
    ),
    ?assertEqual(mysql_proto:parse("select d.uno, d2.dos from data as d, data2 as d2"),
        #select{params = [#key{alias = <<"uno">>,name = <<"uno">>,
                       table = <<"d">>},
                  #key{alias = <<"dos">>,name = <<"dos">>,table = <<"d2">>}],
        tables = [#table{name = <<"data">>,alias = <<"d">>},
                  #table{name = <<"data2">>,alias = <<"d2">>}]}
    ),

    #select{params = [#all{}], tables =[#table{name= <<"streams">>}],
      order = [#order{key= <<"name">>, sort = asc}], limit =1} =
    mysql_proto:parse("SELECT `streams`.* FROM `streams` ORDER BY `streams`.`name` ASC LIMIT 1"),
    ok.

select_from_subquery(_) ->
    ?assertEqual(mysql_proto:parse("select * from (select 1 as uno,2 as dos)"),
        #select{
    params = [#all{}],
    tables = 
        [#subquery{
             subquery = 
                 #select{
                     params = 
                         [#value{name = <<"uno">>,value = 1},
                          #value{name = <<"dos">>,value = 2}]}}]}
    ),
    ?assertEqual(mysql_proto:parse("select (select 1) as id, t.uno from (select 2) as t"),
        #select{
    params = 
        [#subquery{
             name = <<"id">>,
             subquery = 
                 #select{
                     params = [#value{name = undefined,value = 1}]}},
         #key{alias = <<"uno">>,name = <<"uno">>,table = <<"t">>}],
    tables = 
        [#subquery{
             name = <<"t">>,
             subquery = 
                 #select{
                     params = [#value{value = 2}]}}]}
    ),
    ?assertEqual(mysql_proto:parse("select * from clientes where id in ( 1, 2, 3 )"),
        #select{params = [#all{}],
        tables = [#table{name = <<"clientes">>,
                         alias = <<"clientes">>}],
        conditions = #condition{nexo = in,
                                op1 = #key{alias = <<"id">>,name = <<"id">>},
                                op2 = #subquery{subquery = [1,2,3]}}}
    ),
    ok.

select_where(_) ->
    ?assertEqual(mysql_proto:parse("select * from tabla where uno=1"),
        #select{params = [#all{}],
        tables = [#table{name = <<"tabla">>,alias = <<"tabla">>}],
        conditions = #condition{nexo = eq,
                                op1 = #key{alias = <<"uno">>,name = <<"uno">>},
                                op2 = #value{value = 1}}}
    ),
    ?assertEqual(mysql_proto:parse("select * from tabla where uno=1 and dos<2"),
        #select{
    params = [#all{}],
    tables = [#table{name = <<"tabla">>,alias = <<"tabla">>}],
    conditions = 
        #condition{
            nexo = nexo_and,
            op1 = 
                #condition{
                    nexo = eq,
                    op1 = 
                        #key{alias = <<"uno">>,name = <<"uno">>},
                    op2 = #value{value = 1}},
            op2 = 
                #condition{
                    nexo = lt,
                    op1 = 
                        #key{alias = <<"dos">>,name = <<"dos">>},
                    op2 = #value{value = 2}}}}
    ),
    ?assertEqual(mysql_proto:parse("select * from tabla where uno=1 and dos<2 and tres>3"),
        #select{
    params = [#all{}],
    tables = [#table{name = <<"tabla">>,alias = <<"tabla">>}],
    conditions = 
        #condition{
            nexo = nexo_and,
            op1 = 
                #condition{
                    nexo = eq,
                    op1 = 
                        #key{alias = <<"uno">>,name = <<"uno">>},
                    op2 = #value{value = 1}},
            op2 = 
                #condition{
                    nexo = nexo_and,
                    op1 = 
                        #condition{
                            nexo = lt,
                            op1 = 
                                #key{alias = <<"dos">>,name = <<"dos">>},
                            op2 = #value{value = 2}},
                    op2 = 
                        #condition{
                            nexo = gt,
                            op1 = 
                                #key{alias = <<"tres">>,name = <<"tres">>},
                            op2 = #value{value = 3}}}}}
    ),
    ?assertEqual(
        mysql_proto:parse("select * from tabla where uno=1 and dos<=2 and tres>=3"),
        mysql_proto:parse("select * from tabla where uno=1 and (dos=<2 and tres=>3)")
    ),
    ?assertEqual(
        mysql_proto:parse("select * from a where (a=1 and b=2) and c=3"),
        #select{
    params = [#all{}],
    tables = [#table{name = <<"a">>,alias = <<"a">>}],
    conditions = 
        #condition{
            nexo = nexo_and,
            op1 = 
                #condition{
                    nexo = nexo_and,
                    op1 = 
                        #condition{
                            nexo = eq,
                            op1 = #key{alias = <<"a">>,name = <<"a">>},
                            op2 = #value{value = 1}},
                    op2 = 
                        #condition{
                            nexo = eq,
                            op1 = #key{alias = <<"b">>,name = <<"b">>},
                            op2 = #value{value = 2}}},
            op2 = 
                #condition{
                    nexo = eq,
                    op1 = #key{alias = <<"c">>,name = <<"c">>},
                    op2 = #value{value = 3}}}}
    ),
    ok.
    
select_function(_) ->
    ?assertEqual(mysql_proto:parse("select count(*)"), 
        #select{params = [#function{name = <<"count">>, params = [#all{}]}]}
    ),
    ?assertEqual(mysql_proto:parse("select concat('hola', 'mundo')"), 
        #select{params = [#function{name = <<"concat">>,
                            params = [#value{value = <<"hola">>},
                                      #value{value = <<"mundo">>}]}]}
    ),
    ok.

select_groupby(_) ->
    ?assertEqual(mysql_proto:parse("select fecha, count(*) as total from datos group by fecha"),
        #select{params = [#key{alias = <<"fecha">>,
                       name = <<"fecha">>},
                  #function{name = <<"count">>,
                            params = [#all{}],
                            alias = <<"total">>}],
        tables = [#table{name = <<"datos">>,alias = <<"datos">>}],
        group = [<<"fecha">>]}
    ),
    ?assertEqual(mysql_proto:parse("select fecha, count(*) from datos group by fecha"),
        #select{params = [#key{alias = <<"fecha">>,
                       name = <<"fecha">>},
                  #function{name = <<"count">>,
                            params = [#all{}],
                            alias = undefined}],
        tables = [#table{name = <<"datos">>,alias = <<"datos">>}],
        group = [<<"fecha">>]}
    ),
    ?assertEqual(mysql_proto:parse("select * from a group by 1"),
        #select{params = [#all{}],
        tables = [#table{name = <<"a">>,alias = <<"a">>}],
        group = [1]}
    ),
    ok.

select_orderby(_) ->
    ?assertEqual(mysql_proto:parse("select * from tabla order by 1"),
        #select{
            params=[#all{}],
            tables=[#table{alias = <<"tabla">>, name = <<"tabla">>}],
            order=[#order{key=1,sort=asc}]
        }
    ),
    ?assertEqual(mysql_proto:parse("select * from tabla order by 1 desc"),
        #select{
            params=[#all{}],
            tables=[#table{alias = <<"tabla">>, name = <<"tabla">>}],
            order=[#order{key=1,sort=desc}]
        }
    ),
    ok.

select_limit(_) ->
    ?assertEqual(mysql_proto:parse("select * from tabla limit 10"),
        #select{
            params=[#all{}],
            tables=[#table{alias = <<"tabla">>, name = <<"tabla">>}],
            limit=10
        }
    ),
    ?assertEqual(mysql_proto:parse("select * from tabla limit 10 offset 5"),
        #select{
            params=[#all{}],
            tables=[#table{alias = <<"tabla">>, name = <<"tabla">>}],
            limit=10,
            offset=5
        }
    ),
    ok.

select_arithmetic(_) ->
    ?assertEqual(
        #select{params = [#operation{type = <<"+">>,
                             op1 = #value{value = 2},
                             op2 = #value{value = 3}}]},
        mysql_proto:parse("select 2+3")
    ),
    ?assertEqual(
        mysql_proto:parse("select 2+3"),
        mysql_proto:parse("select (2+3)")
    ),
    ?assertNotEqual(
        mysql_proto:parse("select (2+3)*4"),
        mysql_proto:parse("select 2+3*4")
    ),
    ?assertEqual(
        #select{params = [#operation{type = <<"*">>,
                             op1 = #operation{type = <<"+">>,
                                              op1 = #value{value = 2},
                                              op2 = #value{value = 3}},
                             op2 = #value{value = 4}}]},
        mysql_proto:parse("select (2+3)*4")
    ),
    ?assertEqual(
        #select{params = [#all{}],
        tables = [#table{name = <<"data">>,alias = <<"data">>}],
        conditions = #condition{nexo = eq,
                                op1 = #key{alias = <<"a">>,name = <<"a">>},
                                op2 = #operation{type = <<"*">>,
                                                 op1 = #key{alias = <<"b">>,name = <<"b">>},
                                                 op2 = #value{value = 3}}}},
        mysql_proto:parse("select * from data where a = b*3")
    ),
    ok.






update_simple(_) ->
    ?assertEqual(
        mysql_proto:parse("update mitabla set dato=1"),
        #update{
            table=#table{alias = <<"mitabla">>, name = <<"mitabla">>},
            set=[#set{key = <<"dato">>, value=#value{value=1}}]
        }
    ),
    ?assertEqual(
        mysql_proto:parse(" Update   mitabla SET dato  =  1    "),
        mysql_proto:parse("UPDATE mitabla SET dato=1")
    ),
    ok.

update_multiparams(_) ->
    ?assertEqual(
        mysql_proto:parse("update mitabla set dato1=1, dato2='bon jovi', dato3='this ain''t a love song'"),
        #update{
            table=#table{alias = <<"mitabla">>, name = <<"mitabla">>},
            set=[
                #set{key = <<"dato1">>, value=#value{value = 1}},
                #set{key = <<"dato2">>, value=#value{value = <<"bon jovi">>}},
                #set{key = <<"dato3">>, value=#value{value = <<"this ain't a love song">>}}
            ]
        }
    ),
    ok.

update_where(_) ->
    ?assertEqual(
        mysql_proto:parse("update mitabla set dato=1 where dato=5"),
        #update{
            table=#table{alias = <<"mitabla">>, name = <<"mitabla">>},
            set=[#set{key = <<"dato">>, value=#value{value=1}}],
            conditions=#condition{
                nexo=eq, 
                op1=#key{alias = <<"dato">>, name = <<"dato">>},
                op2=#value{value=5}
            }
        }
    ),
    ok.










server_select_simple(_) ->
  {ok, LSocket} = gen_tcp:listen(0, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
  {ok, ListenPort} = inet:port(LSocket),
  Client = spawn_link(fun() ->
    {ok, Sock} = nanomysql:connect("mysql://user:pass@127.0.0.1:"++integer_to_list(ListenPort)++"/dbname"),
    Query1 = "SELECT input,output FROM minute_stats WHERE source='net' AND time >= '2013-09-05' AND time < '2013-09-06'",
    {ok, {Columns1, Rows1}} = nanomysql:execute(Query1, Sock),
    [{<<"input">>,_}, {<<"output">>,_}] = Columns1,
    [
      [<<"20">>,20],
      [<<"30">>,30],
      [<<"40">>,undefined]
    ] = Rows1,
    ok
  end),
  erlang:monitor(process, Client),
  {ok, Sock} = gen_tcp:accept(LSocket),
  My0 = my_protocol:init([{socket,Sock}]),
  {ok, My1} = my_protocol:hello(42, My0),
  {ok, #request{info = #user{}}, My2} = my_protocol:next_packet(My1),
  {ok, My3} = my_protocol:ok(My2),

  {ok, #request{info = #select{} = Select}, My4} = my_protocol:next_packet(My3),
  #select{
    params = [#key{name = <<"input">>},#key{name = <<"output">>}],
    tables = [#table{name = <<"minute_stats">>}],
    conditions = #condition{nexo = nexo_and, 
      op1 = #condition{nexo = eq, op1 = #key{name = <<"source">>},op2 = #value{value = <<"net">>}},
      op2 = #condition{nexo = nexo_and,
        op1 = #condition{nexo = gte, op1 = #key{name = <<"time">>}, op2 = #value{value = <<"2013-09-05">>}},
        op2 = #condition{nexo = lt, op1 = #key{name = <<"time">>}, op2 = #value{value = <<"2013-09-06">>}}
      }
    }
  } = Select,

  ResponseFields = {
    [
      #column{name = <<"input">>, type=?TYPE_VARCHAR, length=20},
      #column{name = <<"output">>, type=?TYPE_LONG, length = 8}
    ],
    [
      [<<"20">>, 20],
      [<<"30">>, 30],
      [<<"40">>, undefined]
    ]
  },
  Response = #response{status=?STATUS_OK, info = ResponseFields},
  {ok, _My5} = my_protocol:send_or_reply(Response, My4),
  receive {'DOWN', _, _, Client, Reason} -> normal = Reason end,
  ok.




server_reject_password(_) ->
  {ok, LSocket} = gen_tcp:listen(0, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
  {ok, ListenPort} = inet:port(LSocket),
  Client = spawn_link(fun() ->
    {error,{1045,<<"password rejected">>}} = nanomysql:connect("mysql://user:pass@127.0.0.1:"++integer_to_list(ListenPort)++"/dbname"),
    ok
  end),
  erlang:monitor(process, Client),
  {ok, Sock} = gen_tcp:accept(LSocket),
  My0 = my_protocol:init([{socket,Sock}]),
  {ok, My1} = my_protocol:hello(42, My0),
  {ok, #request{info = #user{}}, My2} = my_protocol:next_packet(My1),
  {ok, _My3} = my_protocol:error(<<"password rejected">>, My2),

  receive {'DOWN', _, _, Client, Reason} -> normal = Reason end,
  ok.


server_very_long_query(_) ->
  Value = binary:copy(<<"0123456789">>, 2177721),
  Query = iolist_to_binary(["INSERT INTO photos (data) VALUES ('", Value, "')"]),

  {ok, LSocket} = gen_tcp:listen(0, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
  {ok, ListenPort} = inet:port(LSocket),
  Client = spawn_link(fun() ->
    {ok, Sock} = nanomysql:connect("mysql://user:pass@127.0.0.1:"++integer_to_list(ListenPort)++"/dbname"),
    nanomysql:execute(Query, Sock),
    ok
  end),
  erlang:monitor(process, Client),
  {ok, Sock} = gen_tcp:accept(LSocket),
  My0 = my_protocol:init([{socket,Sock},{parse_query,false}]),
  {ok, My1} = my_protocol:hello(42, My0),
  {ok, #request{info = #user{}}, My2} = my_protocol:next_packet(My1),
  {ok, My3} = my_protocol:ok(My2),

  {ok, #request{info = Query}, My4} = my_protocol:next_packet(My3),

  ResponseFields = {
    [#column{name = <<"id">>, type=?TYPE_LONG, length = 8}],
    [[20]]
  },
  Response = #response{status=?STATUS_OK, info = ResponseFields},
  {ok, _My5} = my_protocol:send_or_reply(Response, My4),

  % receive {'DOWN', _, _, Client, Reason} -> normal = Reason end,
  ok.

long_query_2(_) ->
  {ok,_} = test_handler:start_server(long_query_2, 0),
  
  Port = test_handler:existing_port(long_query_2),
  ConnStr = <<"mysql://user:user@127.0.0.1:", (integer_to_binary(Port))/binary, "/test_db?login=init_db">>,
  
  Values0 = binary:copy(<<"'test_name',">>, 500),
  Values = <<Values0/binary, "'test_name'">>,
  Owner = self(),

  Pid = spawn(fun() -> 
    Result = 
    receive
      start -> 
        {ok, Connection} = nanomysql:connect(binary_to_list(ConnStr)),
        nanomysql:execute(<<"select * from test where name in (", Values/binary, ")">>, Connection),
        ok
    after 100 -> 
      {error, not_started}
    end,
    Owner ! Result
  end),

  erlang:monitor(process, Pid),
  Pid ! start,

  ok = receive 
    Msg -> Msg
  after 1000 -> 
    exit(Pid, kill),
    {error, timeout}
  end.


