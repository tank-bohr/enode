-module(enode_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("enode.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    PoolArgs = [
        {name, {local, ?POOL_NAME}},
        {worker_module, enode_worker},
        {size, 20},
        {max_overflow, 0}
    ],
    WorkerArgs = [],
    PoolSpec = poolboy:child_spec(?POOL_NAME, PoolArgs, WorkerArgs),
    {ok, { {one_for_one, 5, 10}, [PoolSpec]} }.
