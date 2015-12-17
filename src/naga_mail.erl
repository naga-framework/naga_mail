-module(naga_mail).
-description('NAGA MAIL OTP Application Server').
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([send/4, send/5, send/6]).
-export([send_template/3, send_template/4]).
-record(state,{opts}).

%% refactor from CB project, todo: send_template
%% ---- server %%
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

%% ----- sen mail template API
send_template(App, Action, Args) -> send_template(App, Action, Args, undefined).
send_template(App, Action, Args, Callback) ->
    case apply(outgoing(), Action, Args) of
        {ok,F,T,H}     -> do_msg(App,F,T,Action,H,[],[],Callback);
        {ok,F,T,H,V}   -> do_msg(App,F,T,Action,H, V,[],Callback);
        {ok,F,T,H,V,O} -> do_msg(App,F,T,Action,H, V, O,Callback);
        {nevermind, Reason} ->
            error_logger:info_msg("Mail Not sent because of ~p", [Reason]),
            {ok, Reason};
        nevermind ->
            error_logger:info_msg("Mail Not sent no reason"),
            ok
    end.

%% ---- internal
env(K)         -> env(?MODULE,K,[]).
env(K,D)       -> env(?MODULE,K,D).
env(A,K,D)     -> application:get_env(A,K,D).
hostname()     -> [{hostname, env(hostname, smtp_util:guess_FQDN())}].
ssl()          -> [{ssl, env(ssl, false)}].
tls()          -> [{tls, env(mail_relay_use_tls, if_available)}].
host()         -> case env(mail_relay_host) of [] ->[];V ->[{relay,V}]end.
port()         -> case env(mail_relay_port) of [] ->[];V ->[{port,V}]end.
credentials()  -> case env(mail_relay_username) of [] -> [{auth,never}]; 
                  Username -> [{auth,always},{username, Username},
                               {password,env(password, "")}] end.
retries()      -> [{retries,env(retries,1)}].
options()      -> ssl()++hostname()++retries()++tls()++host()++port()++credentials().
app()          -> env(app).
to_atom(A)     -> C=lists:concat([atom_to_list(A),"_outgoing_mail"]),list_to_atom(C).
outgoing()     -> env(outgoing,to_atom(app())).
mail_id()      -> (outgoing()):mail_id().
view(App,A,E)  -> M=strings:join([atom_to_list(App),A,E],$_),list_to_atom(M).
render(A,M,V,L)-> M:render([{"_lang",L}|V],[{locale,L},{application,A}]).

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
    % Opts = options(),
    % do_deliver(Opts,F,T,Fun,C).

do_msg(App, F, T, Action, H, V, O, Callback) ->
    BodyFun = fun() -> build_message(App, Action, H, V, O) end,
    gen_server:call(?SERVER, {deliver, F, T, BodyFun, Callback}).
    % Opts = options(),
    % do_deliver(Opts,F,T,Fun,C).

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

%% -----
build_message(App, Action, HeaderFields, Variables, Options) ->
    ContentLanguage         = proplists:get_value("Content-Language", HeaderFields),
    EffectiveAction         = proplists:get_value(template, Options, Action),
    Attachments             = proplists:get_value(attachments, Options, []),
    CharSet                 = proplists:get_value(charset, Options),
    {MimeType, MessageBody} = build_message_body_attachments(App, EffectiveAction, Variables, Attachments, ContentLanguage, CharSet),
    MessageHeader = build_header(HeaderFields, MimeType, CharSet),
    [MessageHeader, "\r\n", ux2dos(MessageBody)].

build_message_body_attachments(App, Action, Variables, [], ContentLanguage, CharSet) ->
    build_message_body(App, Action, Variables, ContentLanguage, CharSet);
build_message_body_attachments(App, Action, Variables, Attachments, ContentLanguage, CharSet) ->
    Boundary = smtp_util:generate_message_boundary(),
    {MimeType, MessageBody} = build_message_body(App, Action, Variables, ContentLanguage, CharSet),
    {"multipart/mixed; boundary=\""++Boundary++"\"",
        render_multipart_view([{MimeType, MessageBody}|Attachments], Boundary, CharSet)}.

build_message_body(App, Action, Variables, ContentLanguage, CharSet) ->
    HtmlResult = render(App, view(App,Action,"html"), Variables, ContentLanguage),
    TextResult = render(App, view(App,Action,"txt"), Variables, ContentLanguage),
    case HtmlResult of
        undefined ->
            case TextResult of undefined -> undefined;
                {ok, TextView} -> {"text/plain", TextView} end;
        {ok, HtmlView} ->
            case TextResult of
                undefined -> {"text/html", HtmlView};
                {ok, TextView} ->
                    Boundary = smtp_util:generate_message_boundary(),
                    {"multipart/alternative; boundary=\""++Boundary++"\"",
                        render_multipart_view(
                            [{"text/plain", TextView},
                                {"text/html", HtmlView}], Boundary, CharSet)}
            end
    end.

render_multipart_view(Parts, Boundary, CharSet) ->
    ["This is a message with multiple parts in MIME format.\r\n",
        render_multipart_view1(Parts, Boundary, CharSet)].

render_multipart_view1([], Boundary, _) ->
    ["--", Boundary, "--"];
render_multipart_view1([{FileName, MimeType, Body}|Rest], Boundary, CharSet) ->
    ["--", Boundary,
        "\r\n", "Content-Type: ", build_content_type(MimeType, CharSet),
        "\r\n", "Content-Disposition: attachment; filename=", FileName,
        "\r\n", "Content-Transfer-Encoding: base64",
        "\r\n\r\n",
        wrap_to_76(base64:encode(erlang:iolist_to_binary(Body))), "\r\n", render_multipart_view1(Rest, Boundary, CharSet)];
render_multipart_view1([{MimeType, Body}|Rest], Boundary, CharSet) ->
    ["--", Boundary, "\r\n", "Content-Type: ", build_content_type(MimeType, CharSet), "\r\n", "Content-Transfer-Encoding: base64", "\r\n\r\n",
        wrap_to_76(base64:encode(erlang:iolist_to_binary(Body))), "\r\n", render_multipart_view1(Rest, Boundary, CharSet)].

wrap_to_76(String)    -> [wrap_to_76(String, [])].
wrap_to_76(<<>>, Acc) -> list_to_binary(lists:reverse(Acc));

wrap_to_76(<<Head:76/binary, Tail/binary>>, Acc) ->
    wrap_to_76(Tail, [<<"\r\n">>, Head | Acc]);
wrap_to_76(Head, Acc) ->
    list_to_binary(lists:reverse([<<"\r\n">>, Head | Acc])).
