-module(enode_worker).

-export([
    start_link/1,
    send_email/2
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

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

send_email(Pid, To) ->
    gen_server:call(Pid, {send_email, To}).

init(_Args) ->
    {ok, undefined}.

handle_call({send_email, To}, _From, State) ->
    {reply, enode:send_email(To), State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
