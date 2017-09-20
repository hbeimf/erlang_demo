-module(hello_client).
-include("hello_thrift.hrl").
-export([test/0]).

p(X)->
    io:format("in the p() ~w~n", [X]),
    ok.

test()->
    Port = 9090,
    {ok, Client0} = thrift_client_util:new("localhost",
            Port,
            hello_thrift, []),
    io:format("~n Client0 : ~p~n", [Client0]),
    {Client1, Res} =  thrift_client:call(Client0, say, ["world"]),
    io:format(" the Res is ~p~n", [Res]),
    io:format("~n Client1 : ~p~n", [Client1]),
    p(Res),
    io:format("the Client0 == Client1: ~p~n", [Client0 == Client1]),
    thrift_client:close(Client1),
    ok.