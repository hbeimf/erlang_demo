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
    MySqlOptions = [{user, "root"}, {password, "123456"}, {database, "test"},
                    {prepare, [{system_account, "select * from system_account limit 1"}]}],
    ChildSpecs = [
        %% MySQL pools
        mysql_poolboy:child_spec(pool1, PoolOptions, MySqlOptions)

        %% other workers...
        % {some_other_worker, {some_other_worker, start_link, []},
        %  permanent, 10, worker, [some_other_worker]}
    ],
    {ok, {{one_for_one, 10, 10}, ChildSpecs}}.    

%%====================================================================
%% Internal functions
%%====================================================================
