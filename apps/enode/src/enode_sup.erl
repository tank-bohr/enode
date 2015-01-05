-module(enode_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_workers/0
]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(WORKERS_COUNT, 20).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_workers() ->
    lists:foreach(fun(_Index) ->
        {ok, _} = supervisor:start_child(enode_workers_sup, [])
    end, lists:seq(1, ?WORKERS_COUNT)).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    WorkQueueSpec = ?CHILD(enode_work_queue, worker),
    WorkerersSupSpec = {enode_workers_sup,
        {supervisor, start_link,
            [{local, enode_workers_sup}, ?MODULE, [workers]]
        },
        permanent, 5000, supervisor, [?MODULE]
    },
    {ok, { {one_for_one, 5, 10}, [
        WorkQueueSpec,
        WorkerersSupSpec
    ]}};
init([workers]) ->
    Spec = {enode_worker,
        {enode_worker, start_link, []},
        temporary, brutal_kill, worker,
        [enode_worker]
    },
    {ok, { {simple_one_for_one, 0, 1}, [Spec]} }.
