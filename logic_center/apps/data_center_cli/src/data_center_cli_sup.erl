%%%-------------------------------------------------------------------
%% @doc data_center_cli top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(data_center_cli_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    % {ok, { {one_for_all, 0, 1}, []} }.
    Server = {data_center_cli_server, {data_center_cli_server, start_link, []},
               permanent, 5000, worker, [data_center_cli_server]},

    Children = [Server],
    {ok, { {one_for_all, 10, 10}, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
