-module(enode).

-export([
    start/0,
    server/0,
    send_email/1,
    send_email/2
]).

-define(FROM, <<"noreply@bohr.su">>).
-define(SMTP_SEVER, local).
%% -define(SMTP_SEVER, real).

start() ->
    application:ensure_all_started(?MODULE).

server() ->
    lager:start(),
    gen_smtp_server:start(smtp_server_example, [[
        {sessionoptions, [
            {allow_bare_newlines, fix},
            {callbackoptions, [
                {parse, true}
            ]}
        ]}
    ]]).

send_email(To) ->
    send_email(To, fun callback/1).

send_email(To, Callback) ->
    {ok, Body0} = email_dtl:render([
        {sender, ?FROM},
        {recipient, To}
    ]),
    Body = iolist_to_binary(Body0),
    lager:debug("body is [~p]", [Body]),
    Options = get_options(To),
    gen_smtp_client:send({?FROM, [To], Body}, Options, Callback).

callback({exit, Error}) ->
    lager:error("exit received: [~p]", [Error]);
callback({error, Type, Message}) ->
    lager:error("error [~p] received: [~p]", [Type, Message]);
callback({ok, Receipt}) ->
    lager:info("ok received: [~p]", [Receipt]).

get_options(To) ->
    case ?SMTP_SEVER of
        local ->
            [
                {relay, "localhost"},
                {port, 2525},
                {no_mx_lookups, true}
            ];
        real ->
            [_User, Host] = string:tokens(To, "@"),
            [{relay, Host}]
    end.
