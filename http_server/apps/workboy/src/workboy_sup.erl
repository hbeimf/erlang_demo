%%%-------------------------------------------------------------------
%% @doc workboy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(workboy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

% start_link() ->
%     supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% %%====================================================================
% %% Supervisor callbacks
% %%====================================================================

% %% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
% init([]) ->
%     {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
    supervisor:start_child(?SERVER, []).

init([]) ->
    Element = {workboy_server, {workboy_server, start_link, []},
               temporary, brutal_kill, worker, [workboy_server]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
