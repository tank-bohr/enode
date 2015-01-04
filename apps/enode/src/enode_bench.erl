-module(enode_bench).

-export([
    start/0
]).

start() ->
    File = filename:join(code:priv_dir(enode), "emails.csv"),
    {ok, IoDevice} = file:open(File, [read, binary]),
    Timer = timer(),
    ok = loop(IoDevice),
    Elapsed = Timer(),
    lager:info("Elapsed ~.2f sec", [Elapsed / 1000000]),
    ok = file:close(IoDevice).

loop(IoDevice) ->
    case file:read_line(IoDevice) of
        {ok, Data} ->
            To = chomp(Data),
            %% lager:debug("email is: [~p]", [To]),
            Result = enode:send_email(To),
            lager:debug("Send email result: [~p]", [Result]),
            loop(IoDevice);
        eof ->
            ok;
        {error, Reason} ->
            lager:error("Error occured while file read: [~p]", Reason),
            ok
    end.

timer() ->
    StartTime = os:timestamp(),
    fun() ->
        timer:now_diff(os:timestamp(), StartTime)
    end.

chomp(Binary) ->
    re:replace(Binary, "\\r?\\n$", <<>>, [{return, binary}]).
