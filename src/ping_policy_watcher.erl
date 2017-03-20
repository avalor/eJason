-module(ping_policy_watcher).

-export([start/5]).

-include("include/macros.hrl").

%% Initiates the ping watcher of a ping policy
start(Supervisor, Supervised, Frequency, PingTime, MaxPings)->
    apply_ping_policy(Supervisor, Supervised, Frequency, PingTime, MaxPings,
		      0).

%% Enforces the ping policy with the corresponding parameters
%% It maintains the counter of consecutive failures in order to
%% trigger the unblock policy by notifying the supervision manager
apply_ping_policy(Supervisor, Supervised, Frequency, PingTime, 
		  MaxPings, ConsecutiveFailures)->
    io:format("[PPW] Pinging ~p~n",[Supervised]),
    {Supervised, node()} ! {signal, {ping, self()}},
    NewFailuresCount =
	receive 
	    pong ->
		timer:sleep(Frequency),
		0
	after 
	    PingTime -> %% Ping failure
		io:format("[PPW] Ping Failure number ~p~n",
			  [ConsecutiveFailures + 1]),
		ConsecutiveFailures + 1
	end,
    if
	NewFailuresCount > MaxPings ->
	    %% Ping Threshold exceeded, the process terminates
	    io:format("[PPW] Ping Threshold exceeded for ~p~n",
		      [Supervised]),
	    ?SM ! {signal, {pang, Supervisor, Supervised,
			   utils:timestamp()}}; 
	true ->
	    apply_ping_policy(Supervisor, Supervised, Frequency, 
			      PingTime, MaxPings, NewFailuresCount)
    end.
 
	    

	
