%% -*- erlang; utf-8 -*-

% COMMON
-record(table, {name, alias}).
-record(all, {table}).
-record(subquery, {name, subquery }).
-record(key, {alias, name, table}).
-record(value, {name, value}).
-record(condition, {nexo, op1, op2}).
-record(function, {name, params, alias}).
-record(operation, {type, op1, op2}).
-record(variable, {name, label, scope}).

% SHOW
-record(show, {type, full, from}).

-type show() :: #show{}.

% SELECT
-record(select, {params, tables, conditions, group, order, limit, offset}).
-record(order, {key, sort}).

-type select() :: #select{}.

% UPDATE
-record(update, {table, set, conditions}).
-record(set, {key, value}).

-type update() :: #update{}.

% DELETE
-record(delete, {table, conditions}).

-type delete() :: #delete{}.

% INSERT
-record(insert, {table, values}).

-type insert() :: #insert{}.

-type sql() :: show() | select() | update() | delete() | insert().
