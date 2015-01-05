-module(enode_worker).
-export([
    start_link/0
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(POLLING_PERIOD, 1000). %% One second

-record(state, {
    tref
}).

start_link() ->
    Args = [],
    gen_server:start_link(?MODULE, Args, []).

init(_Args) ->
    put(worker_id, make_ref()),
    self() ! work,
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(work, State) ->
    {ok, TRef} = work(),
    {noreply, State#state{tref=TRef}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

work() ->
    case enode_work_queue:poll() of
        {#'basic.get_ok'{delivery_tag = Tag}, Content} ->
            #amqp_msg{payload = Payload} = Content,
            ok = do_work(Payload),
            enode_work_queue:ack(Tag),  
            work();
        #'basic.get_empty'{} ->
            sleep()
    end.

do_work(Payload) ->
    Result = enode:send_email(Payload),
    WorkerId = get(worker_id),
    lager:notice("Worker [~p] with [~p]: [~p]", [WorkerId, Payload, Result]),
    ok.

sleep() ->
    timer:send_after(?POLLING_PERIOD, work).
