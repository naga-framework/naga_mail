-module(naga_mail).
-description('NAGA MAIL OTP Application Server').
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([send/4, send/5, send/6]).
-record(state,{opts}).

%% refactor from CB project, todo: send_template
%% ---- server %% fixme: do we need?
start_link()        -> start_link([]).
start_link(Args)    -> gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).
handle_cast(_, S)   -> {noreply, S}.
terminate(_, _)     -> ok.
code_change(_, S, _)-> {ok, S}.
handle_info(_, S)   -> {noreply, S}.

init([]) ->
    O = options(),
    error_logger:info_msg("Starting server ~p with options ~p...",[?SERVER,O]),
    {ok, #state{opts=O}}.

handle_call({deliver, F, T, BodyFun, ResultFun}, _From, #state{opts=C}=State) ->
    {reply, do_deliver(C, F, T, BodyFun, ResultFun), State}.

%% ----- sen mail API: from,to,subject,body,args,callback
send(F,T,S,B)     -> do_send(F,T,S,B,undefined).
send(F,T,S,B,A)   -> send(F,T,S,B,A,undefined).
send(F,T,S,B,A,C) -> do_send(F,T,S,io_lib:format(B,A),C).


%% ---- internal
env(A,K)     -> env(A,K,[]).
env(A,K,D)   -> application:get_env(A,K,D).
hostname()   -> [{hostname, env(naga_mail, hostname, smtp_util:guess_FQDN())}].
ssl()        -> [{ssl, env(naga_mail, ssl, false)}].
tls()        -> [{tls, env(naga_mail, mail_relay_use_tls, if_available)}].
host()       -> case env(naga_mail, mail_relay_host) of [] ->[];V ->[{relay,V}]end.
port()       -> case env(naga_mail, mail_relay_port) of [] ->[];V ->[{port,V}]end.
credentials()-> case env(naga_mail, mail_relay_username) of [] -> [{auth,never}]; 
                  Username -> [{auth,always},{username, Username},
                               {password,env(naga_mail, password, "")}] end.
retries()    -> [{retries,env(naga_mail,retries,1)}].
options()    -> ssl()++hostname()++retries()++tls()++host()++port()++credentials().
app()        -> env(naga_mail,app).
to_atom(A)   -> C=lists:concat([atom_to_list(A),"_outgoing_mail"]),list_to_atom(C).
outgoing()   -> env(naga_mail,outgoing,to_atom(app())).
mail_id()    -> (outgoing()):mail_id().

do_deliver(Options, From, To, BodyFun, ResultFun) ->
    MailOptions = case proplists:lookup(relay, Options) of
                    none -> [_User, Host] = string:tokens(To, "@"),
                            [{relay, Host} | Options];
                    _ -> Options end,
    Email = {From, [To], BodyFun},
    gen_smtp_client:send(Email, MailOptions, ResultFun).

do_send(F,T,S,B,C) ->
    Params = [{"Subject", S},{"To", T},{"From", F}],
    Header = build_header(Params,"text/plain",undefined),
    Fun = fun() -> [Header, "\r\n", ux2dos(B)] end,
    gen_server:call(?SERVER, {deliver,F,T,Fun,C}).
    % O = options(),
    % do_deliver(O,F,T,Fun,C).

build_header(HeaderFields, DefaultMimeType, CharSet) ->
    MessageID   = case proplists:get_value("Message-ID", HeaderFields) of
                    undefined -> mail_id(); Other -> Other end,
    ContentType = build_content_type(proplists:get_value("Content-Type", HeaderFields, DefaultMimeType), CharSet),
    Date        = proplists:get_value("Date", HeaderFields, erlydtl_dateformat:format("r")),
    AllHeaders  = [{"Date", Date}, {"Content-Type", ContentType},
                   {"MIME-Version", "1.0"}, {"Message-ID", MessageID} | HeaderFields],
    add_fields(AllHeaders, [], []).

add_fields([], _, Acc) -> lists:reverse(Acc);
add_fields([{Key, Value}|Rest], Seen, Acc) ->
    case proplists:get_value(Key, Seen) of
        undefined -> add_fields(Rest, [Key|Seen], [[Key, ": ", Value, "\r\n"] | Acc]);
        _ -> add_fields(Rest, Seen, Acc) end.

build_content_type(ContentType, undefined) -> ContentType;
build_content_type(ContentType, CharSet) -> io_lib:format("~s; charset=~s", [ContentType, CharSet]).

ux2dos(Body) when is_binary(Body) -> ux2dos(binary_to_list(Body));
ux2dos(Body) when is_list(Body)   -> ux2dos(Body, []).

ux2dos([], Acc)                   -> lists:reverse(Acc);
ux2dos([$\r, $\n|Rest], Acc)      -> ux2dos(Rest, [$\n, $\r|Acc]);
ux2dos([$\n|Rest], Acc)           -> ux2dos(Rest, [$\n, $\r|Acc]);
ux2dos([H|T], Acc) when 
    is_binary(H); is_list(H)      -> ux2dos(T, [ux2dos(H)|Acc]);
ux2dos([H|T], Acc)                -> ux2dos(T, [H|Acc]).
