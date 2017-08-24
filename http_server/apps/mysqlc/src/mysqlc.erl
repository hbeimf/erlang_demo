-module(mysqlc).

%% API
% -export([start_link/0]).

-compile(export_all).

% mysqlc:insert().

% insert() -> 
% 	Sql = "INSERT INTO `tablename` (`f1`) VALUES (?)",
% 	ParamsList = [1],
% 	insert(pool1, Sql, ParamsList).
insert(Sql, ParamsList) ->
	insert(pool1, Sql, ParamsList).
insert(Pool, Sql, ParamsList) ->
	mysql_poolboy:query(Pool, Sql, ParamsList). 

select() -> 
	Sql = "select * from test",
	select(pool1, Sql).
select(Pool, Sql) ->
	mysql_poolboy:query(Pool, Sql).	 


test() -> 
	select(pool1, "show tables").




