
%% Copyright (c) 2012-2015, Álvaro Fernández Díaz
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

-export([
	 %run_erl/1,
	 run/1, 
	 run/2, 
	 run/3, 
	 %run_test/1, 
	 %run_test/2, 
	 parse/1,
	 kill/1, 
	 kill/2,
	 %kill_test/1, 
	 %kill_test/2, 
	 compile/1, 
	 %load/1,
	 crun/1,
	 crun/2,
	 crun/3,
	 dm/0, 
	 sm/0, 
	 start/0,
	 debug/0,
	 check_mailbox/0
	 %test/1, 
	 %build/1
	]).


-include("include/macros.hrl").
-include("include/variables.hrl").

%run_erl([Name])->
%    run(list_to_atom(Name));
%run_erl([Agent,Name]) ->
%    run(list_to_atom(Agent),list_to_atom(Name)).

%Agent can be an agent code (atom) or optionally include a name (atom)
%and/or a number (integer) of copies

run(error)->
    io:format("Bad format.\nUsage: ~n\trun('AgentCode')"++
		  "or ~n\trun(\['AgentName'|Rest\])~n~n");
    
run([])->
    ok;
run([Agent|Rest])->
    run(Agent),
    run(Rest);
run(AgentCode) when is_atom(AgentCode)->
   run(AgentCode,1);
run(_) ->
    run(error).
%% run(Agent) when is_tuple(Agent)->  
%%     case Agent of
%% 	{Code,Num} when is_number(Num)->
%% 	    run(Code,Code,Num);

%% 	{Code,Name} when is_atom(Name)->
%% 	    run(Code,Name,1);
%% 	{Code,Name,Num} ->
%% 	    run(Code,Name,Num);
%% 	_ when is_atom(Agent)->
%% 	    run(Agent)
%%     end;
%% UNREACHABLE CODE? 16/08/2016
%% run([Agent|List]) ->
%%     run(Agent),
%%     run(List).


run(error,error)-> 
    io:format("Bad format.\nUsage:
    ~n\trun('AgentCode','AgentName')"++ 
		  "or ~n\trun('AgentCode',NumInstances)~n~n");

run(Code,Name) when is_atom(Code), is_atom(Name)->
    run(Code,Name,1);
run(AgentCode,Num) when is_atom(AgentCode), is_number(Num)->
    Code = atom_to_list(AgentCode),
    {_,Name} = utils:split_path(Code),
   
    ReverseName = 
	lists:reverse(Name),

    NewName =
	case string:str(Code,"lsa.") of
	    1 ->
		list_to_atom(lists:reverse(string:substr(ReverseName,5)));
	    _ ->
		list_to_atom(Name)
	end,      


    run(AgentCode, NewName, Num);
run(_,_) ->
    run(error,error).


run(Code,Name,Num) when is_atom(Code), is_atom(Name), is_number(Num)->
    %% The fourth parameter avoids race conditions that make several
    %% agents to have the same name
    run(Code,Name,Num,1);
run(_,_,_) ->
    io:format("Bad format.\nUsage:
    ~n\trun('AgentCode','AgentName', NumInstances)~n~n").

    

run(_,_,Num,_) when Num < 1 ->
    ok;
run(Code,Name,Num,OrderNum) when is_atom(Code),
				 is_atom(Name),
				 is_number(Num)->
    
    UsedNum = register_as(atom_to_list(Code),Name,OrderNum),
    run(Code,Name,Num-1,UsedNum+1).


%% used to spawn several agents from console
%% Returns the last OrderNum used
register_as(Code,Name,OrderNum) when is_list(Code),
				     is_atom(Name),
				     is_number(OrderNum)->
    NewName = case OrderNum of
		  1 ->
		      Name;
		  _ ->
		      erlang:list_to_atom(
			lists:flatten(
			  io_lib:format("~p_~p",[Name,erlang:abs(OrderNum)])))
	      end,

    case whereis(NewName) of
	undefined->
	  
	    actions:internal_action(shell_process_no_agent_info,
			    [],%%No bindings
			    '.',
			    #var{functor = #var{id=create_agent,
						args = ?ISATOM,
						functor = create_agent},
				 id = call_from_shell, 
				 args = {#var{functor = NewName,
					      id = NewName,
					      args = ?ISATOM},
					 #var{functor = Code,
					      id = Code,
					      args = ?ISATOM},
					 #var{functor = no_custom_properties,
					      id = no_custom_properties,
					      args = ?ISATOM}}}),
	    OrderNum;
			  
	    %Pid = spawn(Code,start,[NewName]),
	    %register(NewName,Pid);
	_ ->
	    %% Already exists, use a higher number
	    register_as(Code,Name,OrderNum+1)
    end.




% (re)starts the distribution manager
dm()->
    case whereis(ejason_distribution_manager) of
	undefined->
	    spawn_monitor(ejason_distribution_manager,start,[]);
	Pid ->
	    exit(Pid,kill),
	    timer:sleep(1),
	    dm()
   end.

% (re)starts the supervision manager

sm()->
   case whereis(ejason_supervision_manager) of
       undefined->
	   spawn_monitor(ejason_supervision_manager,start,[]);
       Pid ->
	   exit(Pid,kill),
	   timer:sleep(1),
	   sm()
   end.

% Both above
start()->
    {SMPid, _SMRef} = sm(),
    {DMPid, _DMRef} = dm(),
    io:format("[eJason Managers started with pids: DM ~p  SM ~p]~n",
	      [DMPid, SMPid]).


parse([]) ->
    ok;
parse([AgentCode|List])->
    parse(AgentCode),
    parse(List);
parse(AgentCode) when is_atom(AgentCode)->
    parse({AgentCode, ?DEFAULTENVIRONMENT});
parse({AgentCode,Environment}) when is_atom(AgentCode)->
    try
   
	ReverseName = 
	    lists:reverse(atom_to_list(AgentCode)),
	
	FileName =
	    case string:str(ReverseName,"lsa.") of
		1 ->
		    list_to_atom(lists:reverse(string:substr(ReverseName,5)));
		_ ->
		    AgentCode
	    end,      


	jasonNewParser:parse_agent(FileName, Environment)
    catch 
	error:{badmatch,{error,{Line,jasonGrammar,Reason}}} ->
	    io:format("Error parsing file ~p.asl~n"++
		      "Line: ~p~nReason: ~s~n\n\n",
		      [AgentCode,Line,string:join(Reason," ")]),
	    error;
	error:{badmatch,{error,{Line,erl_scan,Scanning},_}} ->

	    io:format("Error parsing file ~p.asl~n"++
			  "Line: ~p~nReason: error scanning ~p~n"++
			  "Tip: look for some double quotes that are not closed.\n\n\n",
	  	      [AgentCode,Line,Scanning]),
	    error;
	exit:asl_file_not_found ->
	    ok
	  %% A:B ->
	  %%   io:format("A: ~p~nB: ~p~n",[A,B])	    
    end;
parse(_) ->
    io:format("Bad format.\nUsage: ~n\tparse('AgentCode')~n~n").






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



compile([File])->
    compile(File);
compile(File) when is_atom(File)->
    ErlFile = io_lib:format(?OUTPUTDIR++"/~p",[File]),
    try 
	{ok, File} =
	    compile:file(ErlFile,[{i, "./include"}, 
				  {i,"../include/"},
				  {outdir, ?OUTPUTDIR}]),
	
	c:l(File)
     catch
	 _:_ ->
	     io:format("[ejason] Agent in file ~s/~p.erl cannot be compiled~n",[?OUTPUTDIR,File])
     end;
compile([File|Rest]) when is_atom(File)->
    compile(File),
    compile(Rest);
compile(_) ->
    io:format("Bad format.\nUsage: ~n\tcompile('AgentCode') or"++
		  "\tcompile(['AgentCode'|Rest]).~n~n").



% Parse and compile
load(File)->
    parse(File),
    compile(File).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parses the .asl file, compiles and runs the agent
%
% Sample usage: crun('bob.asl', bobby,1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


crun(Code) when is_atom(Code)->
    crun(Code,1);
crun([{Code, Name,Num}]) when is_atom(Code),
				   is_atom(Name),
				   is_number(Num)->
    crun(Code,Name,Num);
crun([{Code, Name,Num}|Rest]) when is_atom(Code),
				   is_atom(Name),
				   is_number(Num)->
    crun(Code,Name,Num),
    crun(Rest);
crun(_) ->
    io:format("Bad format for crun/1.\nUsage: ~n\tcrun('AgentCode')"++
		  " or ~n\tcrun(\[\{'AgentCode', 'AgentName',
		  NumInstances\}|Rest\])~n~n").



crun(Code,Name) when is_atom(Code),
		     is_atom(Name)->
    crun(Code,Name,1);
crun(Code,Num) when is_atom(Code),
		    is_integer(Num)->
    ReverseName = 
	lists:reverse(atom_to_list(Code)),
    
    AgentName =
	case string:str(ReverseName,"lsa.") of
	    1 ->
		list_to_atom(lists:reverse(string:substr(ReverseName,5)));
	    _ ->
		Code
	end,      

    crun(Code, AgentName, Num);
crun(_,_) ->
    io:format("Bad format for crun/2.\nUsage: ~n\tcrun('AgentCode', 'AgentName')"++
  " or ~n\tcrun('AgentCode', NumInstances)~n~n").


crun(Code,Name,Num)when is_atom(Code),
			is_atom(Name),
			is_number(Num)->
    parse(Code),
    %% {Module,Environment} =
    %% 	case Code of
    %% 	_ when is_atom(Code)->
    %% 	    {Code,?DEFAULTENVIRONMENT};
    %% 	{_ModuleName,_Environment}->
    %% 	    Code
    %% 	end,
    compile(Code),
    %compile(Environment),
    run(Code,Name,Num);
crun(_,_,_) ->
    io:format("Bad format for crun/3.\nUsage: ~n\tcrun(\"AgentCode\", 'AgentName', NumInstances)~n~n").










%%%%%%%%%%%%%% ERASEABLE CODE



%% invokes kill, parses, compiles, reloads and runs

%% test(Name) when is_atom(Name)->
%%     test([Name]);
%% test(NameList) when is_list(NameList) ->
%%     lists:map(fun ejason:kill/1, NameList),
%%     lists:map(fun ejason:parse/1, NameList),
%%     lists:map(fun (SomeName) ->
%% 		      compile:file(SomeName, [report_errors, {i, "./include"}, 
%% {i,"../include/"}]),
%% 		      try
%% 			  erlang:purge_module(SomeName)
%% 		      catch
%% 			  _:_ ->
%% 			      o
%% 		      end,
%% 		      code:load_file(SomeName)  end, 
%% 	      NameList),
%%     timer:sleep(1000),
%%     lists:map(fun (SomeName) ->
%% 		      ejason:run(SomeName) end,
%% 	      NameList).

%% %% Parses and compiles
%% build(Name) when is_atom(Name)->
%%     build([Name]);
%% build(NameList) when is_list(NameList) ->
%%     lists:map(fun ejason:kill/1, NameList),
%%     lists:map(fun ejason:parse/1, NameList),
%%     lists:map(fun ejason:compile/1,NameList).
%%     lists:map(fun (SomeName) ->
%% 		      %% io:format("[ejason.erl] Name: ~p~n",[SomeName]),
%% 		      compile:file(SomeName, [report_errors, {i, "./include"}, 
%% {i,"../include/"}]),
%% 		      try
%% 			  erlang:purge_module(SomeName)
%% 		      catch
%% 			  _:_ ->
%% 			      ok
%% 		      end,
%% 		      code:load_file(SomeName)  end, 
	      %% NameList).




%% run_kill([])->
%%     ok;
%% run_kill(Agent) when is_atom(Agent)->
%%     run_kill(Agent,1),
%%     receive 
%% 	{killed,_Pid} ->
%% 	    ok
%%     end;
%% run_kill([Agent|List]) when is_tuple(Agent)->
%%     {Name,Num} = Agent,
%%     run_kill(Name,Num),
%%     run_kill(List),
%%     receive 
%% 	{killed,_Pid} ->
%% 	    ok
%%     end;
%% run_kill([Agent|List]) when is_atom(Agent)->
%%     run_kill(Agent,1),
%%     run_kill(List),
%%     receive 
%% 	{killed,_Pid} ->
%% 	    ok
%%     end.

%% run_kill(_Name,Num) when Num < 1 ->
%%     ok;
%% run_kill(Name,Num) ->
%%     P = spawn(Name,start,[Num]),
%%     P ! {terminate,kill,self()},
%%     run_kill(Name,Num-1),
%%     receive 
%% 	{killed,_Pid} ->
%% 	    ok
%%     end.


%% run_test(T)->
%%     run_kill(T).

%% run_test(T,Num)->
%%     run_kill(T,Num).


%% %% Function only intended for testing purposes
%% kill_test(Agents)->
%%  %   io:format("~p~n",[Agents]),
%%     utils:killAgent_test(Agents).

%% kill_test(Agent,1)->
%%  %   io:format("~p ~p~n",[Agent,1]),
%%     utils:killAgent_test(Agent),
%%  %   io:format("~pWaiting for ~p~n",[self(),Agent]),
%%     receive 
%% 	{killed,_} ->
%% 	    ok
%%     end;
%% kill_test(Agent,Num)->
%%     Name = list_to_atom(
%% 	     lists:flatten(
%% 	       io_lib:format("~p_~p",[Agent,Num-1]))),
%% %    io:format("~p ~p~n",[Name,Num]),
%%     utils:killAgent_test(Name),
%%     kill_test(Agent,Num-1),
%% %    io:format("~pWaiting for ~p~n",[self(),Name]),
%%     receive 
%% 	{killed,_Pid} ->
%% 	    ok
%%     end.


debug()->					%
    erlang:process_flag(trap_exit, true),
    erlang:link(whereis(ejason_distribution_manager)),
    erlang:link(whereis(ejason_supervision_manager)).

check_mailbox()->
    receive
	X ->
	    io:format("Mailbox: ~p~n", [X])
    after 0 ->
	    io:format("Mailbox empty\n")
    end.
