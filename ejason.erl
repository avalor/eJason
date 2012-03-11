-module(ejason).

-export([run/1, run/2, run_test/1, run_test/2, parse/1,kill/1, kill/2,
	kill_test/1, kill_test/2]).

%Agent can be an agent name or a name plus a number of copies
run([])->
    ok;
run(Agent) when is_atom(Agent)->
    run(Agent,1);
run([Agent|List]) when is_tuple(Agent)->
    {Name,Num} = Agent,
    run(Name,Num),
    run(List);
run([Agent|List]) when is_atom(Agent)->
    run(Agent,1),
    run(List).
run(_Name,Num) when Num < 1 ->
    ok;
run(Name,Num) ->
    spawn(Name,start,[Num]),
    run(Name,Num-1).

run_kill([])->
    ok;
run_kill(Agent) when is_atom(Agent)->
    run_kill(Agent,1),
    receive 
	{killed,_Pid} ->
	    ok
    end;
run_kill([Agent|List]) when is_tuple(Agent)->
    {Name,Num} = Agent,
    run_kill(Name,Num),
    run_kill(List),
    receive 
	{killed,_Pid} ->
	    ok
    end;
run_kill([Agent|List]) when is_atom(Agent)->
    run_kill(Agent,1),
    run_kill(List),
    receive 
	{killed,_Pid} ->
	    ok
    end.

run_kill(_Name,Num) when Num < 1 ->
    ok;
run_kill(Name,Num) ->
    P = spawn(Name,start,[Num]),
    P ! {terminate,kill,self()},
    run_kill(Name,Num-1),
    receive 
	{killed,_Pid} ->
	    ok
    end.



parse([]) ->
    ok;
parse(AgentName) when is_atom(AgentName)->
    jasonParser:parseAgents([AgentName]);
parse([AgentName|List]) ->
    parse(AgentName),
    parse(List).




run_test(T)->
    run_kill(T).

run_test(T,Num)->
    run_kill(T,Num).


%
% Agents ->  atom | [atom]
%
kill(Agents)->
    utils:killAgent(Agents).

kill(Agent,1)->
    utils:killAgent(Agent);
kill(Agent,Num)->
    Name = list_to_atom(
	     lists:flatten(
	       io_lib:format("~p_~p",[Agent,Num-1]))),
    utils:killAgent(Name),
    kill(Agent,Num-1).




%% Function only intended for test purposees
kill_test(Agents)->
 %   io:format("~p~n",[Agents]),
    utils:killAgent_test(Agents).

kill_test(Agent,1)->
 %   io:format("~p ~p~n",[Agent,1]),
    utils:killAgent_test(Agent),
 %   io:format("~pWaiting for ~p~n",[self(),Agent]),
    receive 
	{killed,_} ->
	    ok
    end;
kill_test(Agent,Num)->
    Name = list_to_atom(
	     lists:flatten(
	       io_lib:format("~p_~p",[Agent,Num-1]))),
%    io:format("~p ~p~n",[Name,Num]),
    utils:killAgent_test(Name),
    kill_test(Agent,Num-1),
%    io:format("~pWaiting for ~p~n",[self(),Name]),
    receive 
	{killed,_Pid} ->
	    ok
    end.




%% test()->
%%     parseAgents([agent2]).

%% t()-> 
%%     parseAgents([agent3]),
%%     c:c(agent3).
%% r()->
%%     spawn(agent3,start,[]).

