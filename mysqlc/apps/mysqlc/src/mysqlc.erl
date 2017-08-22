-module(mysqlc).

%% API
% -export([start_link/0]).

-compile(export_all).


test() -> 
	mysql_poolboy:query(pool1, "select * from system_account WHERE id=?", [1]).

test1() -> 
	mysql_poolboy:query(pool1, "select * from system_account limit 3").

