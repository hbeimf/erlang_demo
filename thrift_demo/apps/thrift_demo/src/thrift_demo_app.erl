%%%-------------------------------------------------------------------
%% @doc thrift_demo public API
%% @end
%%%-------------------------------------------------------------------

-module(thrift_demo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    thrift_demo_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
