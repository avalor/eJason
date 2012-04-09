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
% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @author Álvaro Fernández Díaz
%% @copyright 2012 Álvaro Fernández Díaz
%% @doc Provides auxiliary functions to eJason

-module(utils).

-compile(export_all).


get_valuation()->
    receive
	{execute,[],AnswerTo} ->
	     %Finished
	    {AnswerTo,[]};
	{execute,Valuation,AnswerTo} when is_list(Valuation)->
     %  io:format("[~p]Obtained list Valuation: ~p~n",[self(),Valuation]),  
	    [First|_Rest] = Valuation,

% It is a list when the valuation is obtained from the execution of a plan
	    {AnswerTo,First};
	{execute,Valuation,AnswerTo} when is_tuple(Valuation)->
   % io:format("Obtained tuple Valuation: ~p~n",[Valuation]),
	    {AnswerTo,Valuation};
	{execute,Valuation,AnswerTo}->
	   % io:format("Obtained invalid Valuation: ~p~n",[Valuation]),
	    {AnswerTo,Valuation}  
    end.
unify_vars(Atom,_Val,_VarNames) when is_atom(Atom)->
    Atom;
unify_vars({Atom,Params,Label},Val,VarNames)->
    %io:format("Valuation:  ~p~n",[Val]),
    %io:format("Get Indexes for ~p, in ~p~n",[Params,VarNames]),
    Indexes = jasonParser:getIndexesForVars(tuple_to_list(Params),VarNames),
    %io:format("Indexes: ~p~n",[Indexes]),
    [NewParams]= makeParams([Val],Indexes),
    %io:format("NewParams: ~p~n",[NewParams]),
    {Atom,NewParams,Label}.    

add_belief(Belief) ->
    %io:format("Belief: ~p~n",[Belief]),
    {add_belief,Belief}.


remove_belief(Belief)->
    {remove_belief,Belief}. 

remove_add_belief(Belief)->
    {remove_add_belief,Belief}.

new_intention_goal(A)->
    {new_intention_goal,A}.

add_achievement_goal(Goal={_Atom,_Terms,_Label})->
    %EventBody = list_to_tuple([added_achievement_goal|tuple_to_list(Goal)]),
 %   io:format("NEW EVENT1: ~p~n For: ~p~n",[EventBody,Goal]),
    EventBody={add_achievement_goal,Goal},
    EventBody;
add_achievement_goal(Goal) when is_atom(Goal) ->
   % EventBody = list_to_tuple([added_achievement_goal|tuple_to_list(Goal)]),
    EventBody={add_achievement_goal,Goal},
  %  io:format("NEW EVENT2: ~p~n",[EventBody]),
    EventBody.


add_test_goal(Goal={_Atom,_Terms,_Label})->
    EventBody={add_test_goal,Goal},
    EventBody;
add_test_goal(Goal) when is_atom(Goal) ->
    EventBody={add_test_goal,Goal},
    EventBody.



%% Returns a list of tuples. 
%% Each tuple is a set of parameters for a single function call
makeParams([Val|Vals],Positions)->
    Params = makeParams([Val|Vals],Positions,
			lists:duplicate(erlang:length([Val|Vals]),[])),
    makeTuples(removeDuplicates(Params)).

makeParams(_Vals,[],Accum)->
    Accum;
makeParams(Vals,[0|Rest],Accum) ->
 % io:format("Vals y Pos: ~p y ~p~n",[Vals,Pos]),
    Nth = lists:duplicate(length(Vals),'_'),
    NewAccum = lists:zipwith(fun(X,Y) -> lists:append(X,[Y]) end,
		  Accum,Nth),
    makeParams(Vals,Rest,NewAccum);
makeParams(Vals,[Pos|Rest],Accum) when Pos > 0->
 %   io:format("Vals y Pos: ~p y ~p~n",[Vals,Pos]),
    Nth = getNths(Pos,Vals),
    NewAccum = lists:zipwith(fun(X,Y) -> lists:append(X,[Y]) end,
		  Accum,Nth),
    makeParams(Vals,Rest,NewAccum);
makeParams(Vals,[Pos|Rest],Accum) when Pos <0 ->
    makeParams(Vals,Rest,Accum).


removeDuplicates([])->
    [];
removeDuplicates([Elem|Elems]) ->
    case lists:member(Elem,Elems) of
	true ->
	    NewList = removeElement(Elem,Elems),
	    [Elem|removeDuplicates(NewList)];
	false ->
	    [Elem|removeDuplicates(Elems)]
    end.

removeElement(_Elem,[])->
    [];
removeElement(Elem,[Elem|List]) ->
    removeElement(Elem,List);
removeElement(Elem,[Elem2|List]) when Elem =/= Elem2->
    [Elem2|removeElement(Elem,List)].

makeTuples([])->
    [];
makeTuples([List|Lists]) ->
    [erlang:list_to_tuple(List)|makeTuples(Lists)].

getNths(_Pos,[])->
    [];
getNths(Pos,[H|T])->
    %io:format("Pos: ~p~nH: ~p~n",[Pos,H]),
    [erlang:element(Pos,H)|getNths(Pos,T)].



updateValuation(Vals,_Bindings,[])->
Vals;
updateValuation([],_Bindings,_Replacements)->
    [];
updateValuation([Val|Vals],Bindings,Replacements)->
   % io:format("UpdateValuation for: ~p~n~p~n~p~n",[Val,Bindings,Replacements]),
     lists:append(genVals(Val,Bindings,Replacements),
		  updateValuation(Vals,Bindings,Replacements)).

%%Generates all possible replacements from a valuation
genVals(_Val,[],_Replacement)->
    [];
genVals(Val,[Binding|Bindings],Replacements)->
    %io:format("Calling replace in ~p with: ~p ~p ~p~n",
%	      [self(),Val,Binding,Replacements]),
    case replace(Val, Binding,  Replacements) of
	[]->
	    genVals(Val,Bindings,Replacements);
	NewTuple ->
	    lists:append([NewTuple],genVals(Val,Bindings,Replacements))
    end.
	
%%Generates a new valuation tuple for a single replacement list
replace([],_BindingTuple,_)->
    [];
replace(ValTuple,_BindingTuple,[])->
    ValTuple;
replace(ValTuple,BindingTuple,[{-1,_BindingPos}|Replacements])->
    replace(ValTuple,BindingTuple,Replacements); %% To be improved
replace(ValTuple,BindingTuple,[{ValPos,BindingPos}|Replacements])->
   % io:format("ValPos: ~p for ~p~nBindingPos:~p for ~p~n",
%	      [ValPos,ValTuple,BindingPos,BindingTuple]),
    OldValTuple = erlang:element(ValPos,ValTuple),
    NewBinding = erlang:element(BindingPos,BindingTuple),
    NewValTuple = case {OldValTuple,NewBinding} of
		      {X,X} ->
			  erlang:setelement(ValPos,ValTuple,NewBinding);
		      {'_',_}->
			  erlang:setelement(ValPos,ValTuple,NewBinding);
		      {_,'_'}->
			  erlang:setelement(ValPos,ValTuple,OldValTuple);
		      _ -> %Only generate tuples that provide a match
			  []
		  end,
    replace(NewValTuple,BindingTuple,Replacements).


%% %% Each time a set of params is received,
%% %% it returns all matching possibilities
%% rulesLoop(Rule)->
%%     %io:format("~p waiting in RulesLoop~n",[self()]),
%%     receive 
%% 	{getAllRes,Params,Peer}->
%% 	    try
%% 	%io:format("Params RuleLoop: ~p~n",[Params]),
%% 		Res = apply(Rule,erlang:tuple_to_list(Params)),
%% 	%io:format("RUlesloop-> Res: ~p for Params: ~p~n",[Res,Params]),
%% 		Peer ! {res,Res}
%% 	    catch
%% 		error:Reason ->
%% 		    %io:format("ERROR IN RULESLOOP~n"),
%% 		    Peer ! {res,[]},
%% 		    {'EXIT in Rule pred', {Reason, erlang:get_stacktrace()}}
%% 	    after
%% 		rulesLoop(Rule)
%% 	    end;
%% 	stop ->
%% 	    %io:format("Terminado ~p~n",[self()]),
%% 	    ok
%%      end.


initialValuation([],_Atom,Params,NumberOfUndefined)->
    %io:format("Params Initval: ~p~n",[Params]),
    List = lists:append(Params,
			lists:duplicate(NumberOfUndefined,'_')),
    %io:format("List: Initval: ~p~n",[List]),
    [erlang:list_to_tuple(List)];
initialValuation([Result|Results],Atom,Params,NumberOfUndefined) ->
    try
%	io:format("Result: ~p~n",[Result]),
	{Atom, NewParams, _Label} = Result,
	[NewParams|
	 initialValuation(Results,Atom,Params,NumberOfUndefined)]
    catch
	error:Reason->
	    H = erlang:element(1,Result),
	    io:format("~p:~n~p does not match ~p in ~p~n",
		      [Reason,Atom,H,Result]),
	    initialValuation(Results,Atom,Params,
			     NumberOfUndefined)
    end.
	
getVarsFromResults([],Acc)->
    Acc;
getVarsFromResults([{_atom,Terms,_Label}|Rest],Acc) ->
    NewAcc = [Terms|Acc],
    getVarsFromResults(Rest,NewAcc).



query_bb(Module,BB,Query,Pars,FunBase)->
   %io:format("BB:~p~nQUERY: ~p~n",[BB,Query]),
    MatchingBaseRes = getVarsFromResults(
			beliefbase:find(Query,BB),[]),
   % io:format("RETRIEVED: ~p~n",[MatchingBaseRes]),
   % io:format("Pars: ~p~n",[Pars]), 
    RuleRes = case Pars of 
		  []->
%TODO: check if this case must be erased.
		      MatchingBaseRes;
		  [_Par|_Rest] ->
		      query_rule(BB,Module,Query,Pars,FunBase)		      
    end,
   %io:format("RuleRes: ~p~n",[RuleRes]), 

    %%REALLY RETURN ALL? CHECK!
    FinalRes = utils:removeDuplicates(lists:append(MatchingBaseRes,RuleRes)),
   %io:format("Returning:~p~n",[FinalRes]),
    FinalRes.


query_rule(BB,Module,{Atom},[],FunBase)->
    F = {Atom,0},
    %io:format("Looking for ~p in: ~n~p~n",[F,FunBase]),

    case lists:member(F, FunBase) of

	false->
	  %  io:format("No rule for: ~p~n",[Atom]),
	    [];   
	true ->

	    RuleFun = Module:Atom(BB),
	   % P1 = spawn(Module,Atom,[BBID]),
	   % io:format("Creado1: ~p~n",[P1]),

	    Res = utils:getAll(RuleFun,[],[]),
	   % io:format("Termo1: ~p~n",[P1]),

	   % P1 ! stop,
	   % io:format("Terminado1: ~p~n",[P1]),

	    Res
    end;
query_rule(BB,Module,{Atom,_Terms,_Label},Pars= [Par|_RestPars],FunBase)->
    
    F = {Atom,size(Par)},
   %io:format("Looking for ~p in: ~n~p~n",[F,FunBase]),
   %io:format("Module: ~p~nAtom: ~p~n",[Module,Atom]),

    case lists:member(F, FunBase) of

	false->
%	    io:format("No rule: ~p~n",[Atom]),
	    [];   
	true ->
   % io:format("Fun will be: ~p:~p(~p)~n",[Module,Atom,BB]),

	    RuleFun = Module:Atom(BB),
	  %  P1 = spawn(Module,Atom,[BBID]),
%	   io:format("RuleFun: ~p~n",[RuleFun]),
	    Res = utils:getAll(RuleFun,Pars,[]),

	 %  io:format("Termo2: ~p~n",[P1]),
%	    io:format("Res for ~p from query_rule: ~p~n",[Pars,Res]),

	  %  P1 ! stop,
	  %  io:format("Terminado2: ~p~n",[P1]),

	    Res
    end.


	
getAll(_,[],Accum)->
   % io:format("~p Accum: ~p~n",[self(),Accum]),
    Accum;
getAll(Fun,[H|T],Accum)->
    Res = apply(Fun,tuple_to_list(H)),
%% TODO: improve it, using function map.
    getAll(Fun,T,lists:append(Res,Accum)).


valuation_and([],_Val2) ->
    [];
valuation_and([Val|_]=Val1,Val2) ->
    NumVars = lists:seq(1,size(Val)), 
    Replacements = lists:zip(NumVars,NumVars),
    utils:updateValuation(Val1,Val2,Replacements).

valuation_or(Val1,Val2)->
    utils:removeDuplicates(lists:append(Val1,Val2)).


replaceNonVarsForVars(Terms,Vars)->
    replaceNonVarsForVars(Terms,Vars,1,[]).


replaceNonVarsForVars([],Vars,_Count,AccTerms)->
    {lists:reverse(AccTerms),Vars};
replaceNonVarsForVars([T|Terms],Vars,Count,AccTerms) ->
    {NewCount,NewTerm,NewVars} = 
 	case T of
 	    {var,_,_VarName}->
 		{Count,T,Vars};
 	    {_Else,Line,Name} ->% TODO: add case for formula...
		[Pos] = jasonParser:getIndexesForVars([Name],Vars),
		SNewName ="EjasonVar"++integer_to_list(Count),
		%io:format("Str= ~s~n",[SNewName]),
		%io:format("Term= ~p~n",[SNewName]),
 		NewName = list_to_atom(SNewName),
 		{Count+1,{var,Line,NewName}, 
		 replacePositionInList(Vars,Pos,NewName)  }
 	end,
    replaceNonVarsForVars(Terms,NewVars,NewCount,[NewTerm|AccTerms]).


replacePositionInList([],_,_)->
    io:format("WRONG REPLACEMENT. INDEX TOO LONG~n"),
    [];
replacePositionInList([_Element|Rest],1,NewElement) ->
    [NewElement|Rest];
replacePositionInList([Element|Rest],N,NewElement) when N > 1->
    [Element|replacePositionInList(Rest,N-1,NewElement)];
replacePositionInList(List,N,_NewElement) when N < 1->
    io:format("WRONG REPLACEMENT. INDEX TOO SHORT~n"),
    List.


execute(Name, _Module,'.',{send,{Receiver,Intention,Message},_})->
    Receiver ! {communication,Name,{Intention,Message}},
    ok;
execute(Name,_module,'.',{print,{String},_})-> 
    %Now only prints one string at a time
    io:format("[~p]: ~s~n",[Name,String]).




%% ;
%% execute(Name,_module,'.',{print,{String},_}) when is_atom(String)-> 
%%     %Now only prints one string at a time
%%     io:format("[~p]: ~p~n",[Name,String]).



register_agent(OrderNum,Pid,Name)->
   % io:format("Name: ~p~nPid: ~p~n",[Name,Pid]),
    case whereis(Name) of
	undefined->
	    register(Name,Pid),
	    Name;
	_ ->
	    NewName = erlang:list_to_atom(
			lists:flatten(
			  io_lib:format("~p_~p",[Name,OrderNum]))),
	    utils:register_agent(OrderNum+1,Pid,NewName)	        
    end.
	    

killAgent([])->
    ok;
killAgent(Agent) when is_atom(Agent)->
    killAgent([Agent]);
killAgent([Agent|Agents]) ->
    try
    Agent ! {terminate,kill}
    catch
	error:badarg-> %% the agent was killed already
	    ok
    end,
    killAgent(Agents).


%%Used only to measure time
killAgent_test([])->
  % io:format("Terminated killAgent~n"),
    ok;
killAgent_test(Agent) when is_atom(Agent)->
    killAgent_test([Agent]);
killAgent_test([Agent|Agents]) ->
        try
	    Agent ! {terminate,kill,self()}
	catch
	    error:badarg->
		ok
	end,
    killAgent_test(Agents).
	


resolve_test_goal({Atom,Terms,_Label},
		   Module,KB,FunNames)->
    query_bb(Module,KB,{Atom,'_','_'},[Terms],FunNames);
resolve_test_goal({Atom},
		   Module,KB,FunNames) ->
    query_bb(Module,KB,{Atom},[],FunNames).

