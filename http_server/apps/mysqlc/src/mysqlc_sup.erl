%%%-------------------------------------------------------------------
%% @doc mysqlc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mysqlc_sup).

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
% init([]) ->
%     {ok, { {one_for_all, 0, 1}, []} }.

init([]) ->
    PoolOptions  = [{size, 10}, {max_overflow, 20}],

    {ok, Pools}  = application:get_env(mysqlc, pools),

    ChildSpecs = lists:foldl(fun({Pool, MySqlOptions}, Reply) -> 
    		[mysql_poolboy:child_spec(Pool, PoolOptions, MySqlOptions)|Reply]
    end, [], Pools),

    {ok, {{one_for_one, 10, 10}, ChildSpecs}}. 


%%====================================================================
%% Internal functions
%%====================================================================
