%%%-------------------------------------------------------------------
%% @doc connectionc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(connectionc_sup).

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

    % 保存 status
    Server = {connectionc_server, {connectionc_server, start_link, []},
               permanent, 5000, worker, [connectionc_server]},

    Children = [Server],
    {ok, { {one_for_all, 10, 10}, Children} }.





%%====================================================================
%% Internal functions
%%====================================================================
