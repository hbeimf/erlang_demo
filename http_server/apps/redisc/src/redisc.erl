-module(redisc).
-compile(export_all).

test() -> 
	set(),
	redisc_get().

redisc_get() ->
    redisc_get("foo").
redisc_get(Key) ->
    redisc_call:q(pool_redis, ["GET", Key]).

set() ->
    set("foo", "bar").
set(Key, Val) ->
    redisc_call:q(pool_redis, ["SET", Key, Val]).

hget(Hash, Key) -> 
    q(["hget", Hash, Key], 3000).
hgetall(Hash) -> 
    redisc_call:q(pool_redis, ["hgetall", Hash]).
    
q(Command) -> 
    redisc_call:q(pool_redis, Command).
q(Command, Timeout) -> 
    redisc_call:q(pool_redis, Command, Timeout).

