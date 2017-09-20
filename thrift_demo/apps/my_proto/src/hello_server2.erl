-module(hello_server2).
-include("hello_thrift.hrl").
-export([start/0, handle_function/2, say/1, stop/1, handle_error/2]).


debug(Info)->
    io:format("Debug info:~s~n",[Info]).

say(Name)->
    io:format("~n Line:~p~n", [?LINE]),
    Sentence = "Hello," ++ Name,
    debug(Sentence),
    BinSentence = list_to_binary(Sentence),
    BinSentence.

start()->
    start(9090).

start(Port)->
    Handler = ?MODULE,
    thrift_socket_server:start([{handler, Handler},
            {service, hello_thrift},
            {port, Port},
            {name, hello_server}]).

stop(Server)->
    thrift_socket_server:stop(Server).


handle_error(_P1, _P2) -> 
    % io:format("error : ~p ~n ", [{P1, P2}]),
    ok.

handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
    case Function of
        say ->
            {reply, say(tuple_to_list(Args))};
        % add function here
        _ ->
            error
    end.