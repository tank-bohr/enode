-module(enode).

-export([
    start/0,
    server/0,
    send_email/1
]).

-define(APP_NAME, ?MODULE).
-define(FROM, <<"noreply@tollady.ru">>).
%% -define(SMTP_SEVER, local).
-define(SMTP_SEVER, real).

start() ->
    application:ensure_all_started(?APP_NAME).

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
    T0 = os:timestamp(),
    {ok, Body0} = email_dtl:render([
        {sender, ?FROM},
        {recipient, To}
    ]),
    T1 = os:timestamp(),
    Body = iolist_to_binary(Body0),
    T2 = os:timestamp(),
    SignedBody = sign_mail_body(Body),
    T3 = os:timestamp(),
    Options = get_options(To),
    T4 = os:timestamp(),
    Result = gen_smtp_client:send_blocking({?FROM, [To], SignedBody}, Options),
    T5 = os:timestamp(),
    lager:debug("========== Timing for [~p] ==========", [To]),
    lager:debug("Template render: [~p] mks",   [timer:now_diff(T1, T0)]),
    lager:debug("Convertion: [~p] mks",        [timer:now_diff(T2, T1)]),
    lager:debug("DKIM calculation: [~.2f] ms", [timer:now_diff(T3, T2) / 1000]),
    lager:debug("DNS lookup: [~p] mks",        [timer:now_diff(T4, T3)]),
    lager:debug("SMTP session: ~.2f sec",      [timer:now_diff(T5, T4) / 1000000]),
    lager:debug("==========================================================================", []),
    Result.

get_options(To) ->
    case ?SMTP_SEVER of
        local ->
            [
                {no_mx_lookups, true},
                {relay, "localhost"},
                {port, 2525}
            ];
        real ->
            Host = email_host(To),
            [{relay, Host}]
    end.

email_host(Email) when is_binary(Email) ->
    email_host(binary_to_list(Email));
email_host(Email) when is_list(Email) ->
    [_User, Host] = string:tokens(Email, "@"),
    Host.

sign_mail_body(MailBody) ->
    File = filename:join(code:priv_dir(?APP_NAME), "private-key.pem"),
    {ok, PrivKey} = file:read_file(File),
    DKIMOptions = [
        {s, <<"default">>},
        {d, list_to_binary(email_host(?FROM)) },
        {private_key, {pem_plain, PrivKey}}
    ],
    mimemail:encode(mimemail:decode(MailBody), [{dkim, DKIMOptions}]).
