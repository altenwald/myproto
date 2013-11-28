%% -*- erlang; utf-8 -*-
-module(select_test).
-author('bombadil@bosqueviejo.net').

-compile(export_all).

% required for eunit to work
-include_lib("eunit/include/eunit.hrl").

-include("sql.hrl").

%%====================================================================
%% Test cases
%%====================================================================

show_test() ->
    ?assertEqual(#show{type=databases}, mysql_proto:parse("SHOW databases")),
    ?assertEqual(#show{type=tables, full = true}, mysql_proto:parse("SHOW FULL tables")),
    ?assertEqual(#show{type=tables, full = false}, mysql_proto:parse("SHOW tables")),
    ?assertEqual(#show{type=fields,full=true,from= <<"streams">>}, mysql_proto:parse("SHOW FULL FIELDS FROM `streams`")),
    ?assertEqual(#show{type=fields,full=false,from= <<"streams">>}, mysql_proto:parse("SHOW FIELDS FROM `streams`")),
    ok.



select_all_test() ->
    ?assertEqual(mysql_proto:parse("select *"), #select{params=[#all{}]}),
    ?assertEqual(mysql_proto:parse("SELECT *"), #select{params=[#all{}]}),
    ?assertEqual(mysql_proto:parse(" Select    *   "), #select{params=[#all{}]}),
    ok.

select_strings_test() ->
    ?assertEqual(mysql_proto:parse("select 'hola''mundo'"),
        #select{params = [#value{value = <<"hola'mundo">>}]}
    ),
    ok.

select_simple_test() ->
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

select_simple_multiparams_test() ->
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

select_simple_subquery_test() ->
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

select_from_test() ->
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
    ok.

select_from_subquery_test() ->
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

select_where_test() ->
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
    
select_function_test() ->
    ?assertEqual(mysql_proto:parse("select count(*)"), 
        #select{params = [#function{name = <<"count">>, params = [#all{}]}]}
    ),
    ?assertEqual(mysql_proto:parse("select concat('hola', 'mundo')"), 
        #select{params = [#function{name = <<"concat">>,
                            params = [#value{value = <<"hola">>},
                                      #value{value = <<"mundo">>}]}]}
    ),
    ok.

select_groupby_test() ->
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

select_orderby_test() ->
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

select_limit_test() ->
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

select_arithmetic_test() ->
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

