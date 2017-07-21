-module(redis).
-compile(export_all).

get() ->
    redis_call:q(pool1, ["GET", "foo"]).

set() ->
    redis_call:q(pool1, ["SET", "foo", "bar"]).



lpush() ->
    lpush("list100", "val100").
lpush(ListName, Val) ->
    redis_call:q(pool1, ["LPUSH", ListName, Val]).

rpop() ->
    rpop("list100").
rpop(ListName) ->
    redis_call:q(pool1, ["RPOP", ListName]).

% (http_server@127.0.0.1)1> redis:lpush().
% {ok,<<"2">>}
% (http_server@127.0.0.1)2> redis:rpop().
% {ok,<<"value222">>}
% (http_server@127.0.0.1)3> redis:rpop().
% {ok,<<"val100">>}
% (http_server@127.0.0.1)4> redis:rpop().
% {ok,undefined}
% (http_server@127.0.0.1)5> redis:rpop().
% {ok,undefined}
% (http_server@127.0.0.1)6> redis:lpush().
% {ok,<<"1">>}
% (http_server@127.0.0.1)7> redis:lpush().
% {ok,<<"2">>}
% (http_server@127.0.0.1)8> redis:rpop().
% {ok,<<"val100">>}
% (http_server@127.0.0.1)9> redis:rpop().
% {ok,<<"val100">>}
% (http_server@127.0.0.1)10> redis:rpop().
% {ok,undefined}
% (http_server@127.0.0.1)11>
