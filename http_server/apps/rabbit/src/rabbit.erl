-module(rabbit).
-compile(export_all).

-include_lib("amqp_client/include/amqp_client.hrl").

emit_log() ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"logs">>,
                                                   type = <<"fanout">>}),

    Message = <<"info: Hello World!">>,

    amqp_channel:cast(Channel,
                      #'basic.publish'{exchange = <<"logs">>},
                      #amqp_msg{payload = Message}),
    io:format(" [x] Sent ~p~n", [Message]),
    ok = amqp_channel:close(Channel),
    ok = amqp_connection:close(Connection),
    ok.




receive_logs() ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"logs">>,
                                                   type = <<"fanout">>}),

    #'queue.declare_ok'{queue = Queue} =
        amqp_channel:call(Channel, #'queue.declare'{exclusive = true}),

    amqp_channel:call(Channel, #'queue.bind'{exchange = <<"logs">>,
                                             queue = Queue}),

    io:format(" [*] Waiting for logs. To exit press CTRL+C~n"),

    amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue,
                                                     no_ack = true}, self()),
    receive
        #'basic.consume_ok'{} -> ok
    end,
    loop(Channel).

loop(Channel) ->
    receive
        {#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
            io:format(" [x] ~p~n", [Body]),
            loop(Channel)
    end.

% 目前，这个函数是可以工作的, 对应 receive
receive_xx() ->
    {ok, Connection} =
        amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'queue.declare'{queue = <<"hello">>}),
    io:format(" [*] Waiting for messages. To exit press CTRL+C~n"),

    amqp_channel:subscribe(Channel, #'basic.consume'{queue = <<"hello">>,
                                                     no_ack = true}, self()),
    receive
        #'basic.consume_ok'{} -> ok
    end,
    loop(Channel).


% loop(Channel) ->
%     receive
%         {#'basic.deliver'{}, #amqp_msg{payload = Body}} ->
%             io:format(" [x] Received ~p~n", [Body]),
%             loop(Channel)
%     end.



