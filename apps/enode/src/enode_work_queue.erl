-module(enode_work_queue).
-export([
    start_link/0,
    enqueue/1,
    poll/0,
    ack/1
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

-define(SERVER, ?MODULE).

-record (state, {
  connection,
  channel,
  queue
}).

start_link() ->
    Args = [],
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

enqueue(Payload) ->
    gen_server:call(?SERVER, {enqueue, Payload}).

poll() ->
    gen_server:call(?SERVER, poll).

ack(Tag) ->
    gen_server:call(?SERVER, {ack, Tag}).

init(_Args) ->
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{}),
    {ok, #state {
        connection = Connection,
        channel = Channel,
        queue = Queue
    }}.

handle_call({enqueue, Payload}, _From, State) ->
    {reply, do_enqueue(Payload, State), State};
handle_call(poll, _From, State) ->
    {reply, do_poll(State), State};
handle_call({ack, Tag}, _From, State) ->
    {reply, do_ack(Tag, State), State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{channel=Channel, connection=Connection}) ->
    ok = amqp_channel:close(Channel),
    ok = amqp_connection:close(Connection).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_enqueue(Payload, #state{channel=Channel, queue=Queue}) ->
    Publish = #'basic.publish'{exchange = <<>>, routing_key = Queue},
    Props = #'P_basic'{delivery_mode = 2}, %% persistent message
    Msg = #amqp_msg{props = Props, payload = Payload},
    amqp_channel:cast(Channel, Publish, Msg).

do_poll(#state{channel=Channel, queue=Queue}) ->
    Get = #'basic.get'{queue = Queue},
    amqp_channel:call(Channel, Get).

do_ack(Tag, #state{channel=Channel}) ->
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}).
