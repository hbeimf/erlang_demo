-module(example_client).

%% need this to get access to the records representing Thrift message
%% defined in thrift/example.thrift:
-include("example_constants.hrl").

-include("exampleService_thrift.hrl").

-export([request/4, request/0]).

request() -> 
	request("localhost", 9009, 123, "str msg!!"),
	% request("127.0.0.1", 9999, 456, "str msg!!"),
	ok.

request(Host, Port, Id, Msg) ->
    Req = #message{id = Id, text = Msg},
    {ok, Client} = thrift_client_util:new(Host, Port, exampleService_thrift, []),

    %% "hello" function per our service definition in thrift/example.thrift:
    {ClientAgain, Response} = thrift_client:call(Client, hello, [Req]),
    thrift_client:close(ClientAgain),

    io:format("reply: ~p ~n", [Response]),
    Response.
    % ok.