-module(naga_mail).
-description('NAGA MAIL OTP Application Server').
-behaviour(supervisor).
-behaviour(application).
-export([start/2, stop/1, init/1]).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start(_,_) -> supervisor:start_link({local,naga_mail},naga_mail,[]).
stop(_)    -> ok.
init([])   -> 
              { ok, { { one_for_one, 5, 10 }, 
	                [
                         ?CHILD(naga_mail, worker, [])
	                ] 
              }}.
