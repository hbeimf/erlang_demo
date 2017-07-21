-module(redis).
-compile(export_all).

get() ->
    % redis:q(pool1, ["SET", "foo", "bar"]).
    redis_call:q(pool1, ["GET", "foo"]).


