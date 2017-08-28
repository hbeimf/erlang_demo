-module(table_ets).

-define(TABLE, table_test).

-record(rtable, {
    key,
    val
}).


-compile(export_all).

% http://www.iteye.com/topic/689089
% http://erlang.org/doc/man/ets.html#match_delete-2
init() -> 
	 ets:new(?TABLE, [public,set,named_table,{keypos, #rtable.key}]),
	 insert(),
	 lookup().

insert() -> 
	Data = #rtable{key="key", val="val"},
	ets:insert(?TABLE, Data).

lookup() ->
	ets:lookup(?TABLE, "key").

update() -> 
	ets:update_element(?TABLE, "key", {#rtable.val, "new val"}). 

delete() -> 
	ets:delete(?TABLE, "key").

















