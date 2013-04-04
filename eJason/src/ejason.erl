
%% Copyright (c) 2012, Álvaro Fernández Díaz
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @author Álvaro Fernández Díaz
%% @copyright 2012 Álvaro Fernández Díaz
%% @doc Represents the API to eJason, providing means to translate and run
%%      eJason agents.

-module(ejason).

-export([run_erl/1,run/1, run/2, run/3, run_test/1, run_test/2, parse/1,kill/1, kill/2,
	kill_test/1, kill_test/2, compile/1, load/1,crun/1,crun/2,crun/3,
	dm/0, sm/0, start/0]).


-include("macros.hrl").

run_erl([Name])->
    run(list_to_atom(Name));
run_erl([Agent,Name]) ->
    run(list_to_atom(Agent),list_to_atom(Name)).

%Agent can be an agent name or a name plus a number of copies
run([])->
    ok;
run(Agent) when is_atom(Agent)->
    run(Agent,Agent,1);
run(Agent) when is_tuple(Agent)->  
    case Agent of
	{Code,Num} when is_number(Num)->
	    run(Code,Code,Num);

	{Code,Name} when is_atom(Name)->
	    run(Code,Name,1);
	{Code,Name,Num} ->
	    run(Code,Name,Num);
	_ when is_atom(Agent)->
	    run(Agent)
    end;
run([Agent|List]) ->
    run(Agent),
    run(List).



run(Code,NumOrName)->
    run([{Code,NumOrName}]).


run(_,_,Num) when Num < 1 ->
    ok;
run(Code,Name,Num) when is_number(Num)->
    register_as(Code,Name,Num),
    run(Code,Name,Num-1).


 %used to spawn several agents from console
register_as(Code,Name,OrderNum) ->
    NewName = case(OrderNum) of
		  1 ->
		      Name;
		  _ ->
		      erlang:list_to_atom(
			lists:flatten(
			  io_lib:format("~p_~p",[Name,erlang:abs(OrderNum)])))
	      end,

    case whereis(NewName) of
	undefined->
	    utils:execute(no_name,no_module,'.',
			  {create_agent, {NewName,
					  Code,no_custom}, no_label});
			  
	    %Pid = spawn(Code,start,[NewName]),
	    %register(NewName,Pid);
	_ ->
	    register_as(Code,Name,OrderNum+1)
    end.


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


% (re)starts the distribution manager
dm()->
   case whereis(ejason_distribution_manager) of
       undefined->
	   ok;
       Pid ->
	   exit(Pid,kill),
	   timer:sleep(10)
   end,

   spawn(ejason_distribution_manager,start,[]).

% (re)starts the supervision manager

sm()->
   case whereis(ejason_supervision_manager) of
       undefined->
	   ok;
       Pid ->
	   exit(Pid,kill),
	   timer:sleep(10)

   end,
   spawn(ejason_supervision_manager,start,[]).

% Both above
start()->
    sm(),
    dm().

parse([]) ->
    ok;
parse(AgentName) when is_atom(AgentName)->
    jasonNewParser:parseAgents([AgentName]);
parse({AgentName,Environment})->
    jasonNewParser:parseAgents([{AgentName,Environment}]);
parse(AgentNameList) when is_list(AgentNameList) ->
    jasonNewParser:parseAgents(lists:flatten(AgentNameList)).




run_test(T)->
    run_kill(T).

run_test(T,Num)->
    run_kill(T,Num).


%
% Agents ->  atom | [atom]
%
kill(Agents)->
    utils:killAgent(Agents).

kill(Agent,0)->
    utils:killAgent(Agent);
kill(Agent,Num)->
    Name = list_to_atom(
	     lists:flatten(
	       io_lib:format("~p_~p",[Agent,Num]))),
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


compile(File)->
    compile:file(File,[report_errors]),
    c:l(File).


% Parse and compile
load(File)->
    parse(File),
    compile(File).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parses the .asl file, compiles and runs the agent
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

crun(File)->
    crun(File,1).

crun(File,Name) when is_atom(Name)->
    crun(File,Name,1);
crun(File,Num)when is_integer(Num)->
    Module = case File of
		 _ when is_atom(File)->
		     File;
		 {ModuleName,_Environment}->
		     ModuleName
	     end,
    crun(File,Module,Num).


crun(File,Name,Num)->
    parse(File),
    {Module,Environment} =
	case File of
	_ when is_atom(File)->
	    {File,default_environment};
	{_ModuleName,_Environment}=A->
	    A
	end,
    compile(Module),
    compile(Environment),
    run(Module,Name,Num).
