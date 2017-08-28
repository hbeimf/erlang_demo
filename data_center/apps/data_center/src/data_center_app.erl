%%%-------------------------------------------------------------------
%% @doc data_center public API
%% @end
%%%-------------------------------------------------------------------

-module(data_center_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    data_center_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================