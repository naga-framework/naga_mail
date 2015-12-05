-module(naga_smtp).
-behaviour(gen_smtp_server_session).
-export([start_link/0]).
-export([init/4, handle_HELO/2, handle_EHLO/3, handle_MAIL/2, handle_MAIL_extension/2,
        handle_RCPT/2, handle_RCPT_extension/2, handle_DATA/4, handle_RSET/1, handle_VRFY/2,
        handle_other/3, code_change/3, terminate/2]).

-include("naga_mail.hrl").

-record(state, {
	app,incoming,outgoing,
    options = [] :: list(),
    peer :: tuple(),
    banner :: string(),
    hostname :: binary(),
    helo :: binary(),
    rcpt :: binary(),
    from :: binary(),
    to   :: binary()
}).

port()           -> {port,    env(naga_mail,smtp_server_port,     2525)}.
domain()         -> {domain,  env(naga_mail,smtp_server_domain,   "localhost")}.
ip()             -> {address, env(naga_mail,smtp_server_address,  {0, 0, 0, 0})}.
protocol()       -> {protocol,env(naga_mail,smtp_server_protocol, tcp)}.
env(A,K,D)       -> application:get_env(A,K,D).

start_link()     -> A = [port(),domain(),ip(),protocol()],start_link([A]).
start_link(Args) -> gen_smtp_server:start_link({local, ?MODULE}, ?MODULE, Args).

init(Hostname, SessionCount, PeerName, Options) ->
    case SessionCount > 20 of false ->
            App = wf:config(naga_mail,app, naga_mail),
            In  = wf:atom([App,incoming_mail]),
            Out = wf:atom([App,outgoing_mail]),
            State = #state{options = Options, peer=PeerName, hostname=Hostname, app=App, incoming=In, outgoing=Out},
            Banner = io_lib:format("~s ESMTP Naga ~s", [State#state.hostname, ?NAGA_VERSION]),
            {ok, Banner, State};
        true -> error_logger:info_msg("SMTP Connection limit exceeded (~p)", [SessionCount]),
            {stop, normal, io_lib:format("421 ~s is too busy to accept mail right now", [Hostname])}
    end.

handle_HELO(Hostname, State)             -> {ok, 655360, State#state{helo=Hostname}}.  % 640kb of HELO should be enough for anyone. % If {ok, State} was returned here, we'd use the default 10mb limit
handle_EHLO(Hostname, Extensions, State) -> #state{options=Opts}=State,MyExtensions = case proplists:get_value(auth, Opts, false) of 
    					                    true -> advertise(Extensions); false -> Extensions end, 
    					                    {ok, MyExtensions, State#state{helo=Hostname}}.
handle_MAIL(From, State)                 -> authorize(State#state{from=From}).
handle_MAIL_extension(_Extension, State) -> {ok, State}.
handle_RCPT(To, State)                   -> recipient(State#state{to=To}). % Check if the "To" address exists % Check domain, check addressee in domain. % For bounces: % - To = <noreply+MSGID@example.org>  % - Return-Path header should be present and contains <>
handle_RCPT_extension(_Extension, State) -> {ok, State}.
handle_RSET(State)                       -> reset_state(State). % reset any relevant internal state
handle_VRFY(_Address, State)             -> {error, "252 VRFY disabled by policy, just send some mail", State}.
handle_other(Verb, _Args, State)         -> {lists:flatten(io_lib:format("500 Error: command not recognized : '~s'", [Verb])), State}.
code_change(_OldVsn, State, _Extra)      -> {ok, State}.
terminate(Reason, State)                 -> {ok, Reason, State}.
handle_DATA(From, To, Data, State)       -> #state{incoming=In}=State, MsgId = In:mail_id(),
                                            DataRcvd = add_received_header(Data, MsgId, State),
                                            decode_and_receive(MsgId, From, To, DataRcvd, State).


advertise(Ext)      -> Ext ++ [{"AUTH", "PLAIN LOGIN CRAM-MD5"}, {"STARTTLS", true}].
reset_state(State)  -> State#state{helo=undefined, rcpt=undefined, from=undefined}.
decode(Data)        -> try {ok, mimemail:decode(Data)} catch Type:Reason -> {error, {Type,Reason}} end.
authorize(State)    -> #state{incoming=In, peer=Peer, from=From} =State,
		               case In:authorize(From, Peer) of false -> {error, "552 go away", State}; true  -> {ok, State} end.
recipient(State)    -> #state{incoming=In, to=To, peer=Peer} = State,
                       case In:recipient(To, Peer) of true -> {ok,State}; _ -> {error, "550 No such recipient", State} end. 

decode_and_receive(MsgId, From, To, DataRcvd, State) -> 
    decode_and_receive(MsgId, From, To, DataRcvd, State, decode(DataRcvd)).
decode_and_receive(_, _, _, _, State, {error,Reason}) ->
    wf:error(?MODULE,"SMTP receive: Message decode FAILED with ~p", [Reason]),
    {error, "550 Your email cannot be parsed", State};
decode_and_receive(MsgId, From, To, DataRcvd, State, {ok, Decoded}) ->
    In = State#state.incoming,
    Received = In:receive_data(Decoded, From, To, DataRcvd),
    reply_handled_status(Received, MsgId, reset_state(State)).

reply_handled_status(Received, MsgId, State) ->
    KnownHosts = [ X || X <- Received, X =/= {error, unknown_host} ],
    Handled    = [ X || X <- KnownHosts, X =/= undefined ],
    case {KnownHosts, Handled} of
        {[], _} -> {error, "551 User not local. Relay denied.", State};
        {_, []} -> {error, "550 No such user here", State};
        {_, _}  -> {ok, MsgId, State}
    end.    


%% TODO: Add 'for user@example.com' header (depends on recipient, we might have multiple)
%% TODO: Add Return-Path
add_received_header(Data, MsgId, State) ->
    iolist_to_binary([
        <<"Received:">>,
        <<" from [">>, inet_parse:ntoa(State#state.peer), <<"] (helo=">>, filter_string(State#state.helo), <<")">>,
        <<"\r\n\tby ">>, State#state.hostname, <<" with ESMTP (Naga ">>, ?NAGA_VERSION, <<")">>,
        <<"\r\n\t(envelope-from <">>, filter_string(State#state.from), <<">)">>, 
        <<"\r\n\tid ">>, MsgId, 
        %%FIXME
        <<"; ">>, %%z_dateformat:format(calendar:local_time(), "r", []),
        <<"\r\n">>,
        Data
    ]).

filter_string(S) ->
    lists:filter(fun(C) ->
                            C >= 32 
                    andalso C =< 126
                    andalso C =/= $[
                    andalso C =/= $]
                    andalso C =/= $(
                    andalso C =/= $)
                    andalso C =/= $<
                    andalso C =/= $>
                    andalso C =/= $\\
                 end,
                 to_list(S)).

-define(IS_STRING(Term), (is_list(Term) andalso Term /= [] andalso is_integer(hd(Term)))).

to_list(L) when ?IS_STRING(L)      -> L;
to_list(L) when is_list(L)         -> SubLists = [inner_to_list(X) || X <- L], lists:flatten(SubLists);
to_list(A)                         -> inner_to_list(A).

inner_to_list(A) when is_atom(A)   -> atom_to_list(A);
inner_to_list(B) when is_binary(B) -> binary_to_list(B);
inner_to_list(I) when is_integer(I)-> integer_to_list(I);
inner_to_list(L) when is_tuple(L)  -> lists:flatten(io_lib:format("~p", [L]));
inner_to_list(L) when is_list(L)   -> L;
inner_to_list(F) when is_float(F)  -> float_to_list(F,[{decimals,9},compact]).
