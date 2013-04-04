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

-include("parser.hrl").
-include("variables.hrl").
-include("macros.hrl").
-include("suspendedActionsRecords.hrl").



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
   % io:format("NewParams: ~p~n",[NewParams]),
    {Atom,NewParams,Label}.    


%% TODO: remove useless parameters
add_belief(_,Belief) ->
 %   io:format("O: ~p~nBelief: ~p~n",[O,Belief]),
    #event{type = ?ADDBELIEF,
	   body = Belief}.


remove_belief(_,Belief)->
    %io:format("UTILS: Remove Belief: ~p~n",[Belief]),
     #event{type = ?REMOVEBELIEF,
	   body = Belief}.


remove_add_belief(_,Belief)->
      #event{type = ?REMOVEADDBELIEF,
	   body = Belief}. 

new_intention_goal(_,Goal)->
    #event{type = ?ADDINTENTIONGOAL,
	   body = Goal}.


add_achievement_goal(_,Goal)->
    #event{type = ?ADDACHGOAL,
	   body = Goal}.

    %EventBody = list_to_tuple([added_achievement_goal|tuple_to_list(Goal)]),
 %   io:format("NEW EVENT1: ~p~n For: ~p~n",[EventBody,Goal]),
 %   EventBody={add_achievement_goal,Goal},
  %  EventBody.
%add_achievement_goal(Goal) when is_atom(Goal) ->
   % EventBody = list_to_tuple([added_achievement_goal|tuple_to_list(Goal)]),
 %   EventBody={add_achievement_goal,Goal},
  %  io:format("NEW EVENT2: ~p~n",[EventBody]),
  %  EventBody.


add_test_goal(_,Goal)->
    #event{type = ?ADDTESTGOAL,
	   body = Goal}.
%add_test_goal(_,Goal) when is_atom(Goal) ->
 %      #event{type = ?ADDTESTGOAL,
%	   body = Goal}.
    
 
%% Used to deal with queries that access the 
%% private information from the erlang runtime 
add_ejason_private_query(Query)->
    {add_ejason_private_query, Query}.




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
	error:_Reason->
	    H = erlang:element(1,Result),
%	    io:format("~p:~n~p does not match ~p in ~p~n",
%		      [Reason,Atom,H,Result]),
	    initialValuation(Results,Atom,Params,
			     NumberOfUndefined)
    end.



	
getVarsFromResults([],Acc)->
    Acc;
getVarsFromResults([{_atom,Terms,_Label}|Rest],Acc) ->
    NewAcc = [Terms|Acc],
    getVarsFromResults(Rest,NewAcc).



query_bb(Module,BB,Query,Pars,FunBase)->
   %io:format("BBs:~p~nQUERY: ~p~n",[BB,Query]),
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
 %   io:format("Looking for ~p in: ~n~p~n",[F,FunBase]),

    case lists:member(F, FunBase) of

	false->
	  %  io:format("No rule for: ~p~n",[Atom]),
	    [];   
	true ->
%    io:format("Fun will be: ~p:~p(~p)~n",[Module,Atom,BB]),

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
 %  io:format("Module: ~p~nAtom: ~p~n",[Module,Atom]),

    case lists:member(F, FunBase) of

	false->
%	    io:format("No rule: ~p~n",[Atom]),
	    [];   
	true ->
    %io:format("Fun will be: ~p:~p(~p)~n",[Module,Atom,BB]),

	    RuleFun = Module:Atom(BB),
%	    io:format("Pars in query_rule: ~p~n",[Pars]),

%	   io:format("RuleFun: ~p~n",[RuleFun]),
	    Res = utils:getAll(RuleFun,Pars,[]),


	 %  io:format("Termo2: ~p~n",[P1]),
	  %  io:format("Res from query_rule: ~p~n",[Res]),
	  %  P1 ! stop,
	  %  io:format("Terminado2: ~p~n",[P1]),
	    Res
    end.


%% UP: ALL THIS GOES TO BELIEF BASE
	
getAll(_,[],Accum)->
   % io:format("~p Accum: ~p~n",[self(),Accum]),
    Accum;
getAll(Fun,[H|T],Accum)->
    %io:format("Applying Fun: ~p to ~p~n",[Fun,H]),
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


%% Terms: a list of term tuples (often from params)
%% Vars: a list of Varnames
%% Changes all nonvar terms ({atom,line,name}) with a new 
%% Varname ({var,line,newname} and updates
%% the list of varnames.
%% Returns a tuple {newterms, newvarnames}
replaceNonVarsForVars(Terms,Vars)->
    %io:format("Terms: ~p~n",[Terms]),
    %io:format("VarNames: ~p~n",[Vars]),
    replaceNonVarsForVars(Terms,Vars,1,[]).


replaceNonVarsForVars([],Vars,_Count,AccTerms)->
    {AccTerms,Vars};
replaceNonVarsForVars([T|Terms],Vars,Count,AccTerms) ->
  %  io:format("Term: ~p~n",[T]),
%%TODO: all these should be done in a preprocessing stage.
    {NewCount,NewTerms,NewVars} = 
 	case T of
 	    {var,_,_VarName}->
 		{Count,[T],Vars};
 	    {atom,_Line,Name} ->
		%[Pos] = jasonParser:getIndexesForVars([Name],Vars),
		SNewName ="EjasonVar"++integer_to_list(Count),
 		NewName = list_to_atom(SNewName),
 		{Count+1, 
		 %replacePositionInList(Vars,Pos,NewName)};
		 replaceTermWith(Name, NewName,Terms),
		 replaceTermWith(Name, NewName,Vars)};
	    
	    {formula,_FName,Params,Label} ->
		{ParamsNewTerms,ParamsNewVars} =
		    replaceNonVarsForVars(Params,Vars,Count,[]),
		{LabelNewTerms,LabelNewVars} =
		    replaceNonVarsForVars(Label,ParamsNewVars,
					  Count+length(ParamsNewVars),[]),
       
		Count2 = Count+length(ParamsNewVars)+length(LabelNewVars),

%		[Pos] = 
%		    jasonParser:getIndexesForVars(
%		      jasonParser:removeParamTypes([T]),
%						      Vars),
%		io:format("Pos is ~p~n",[Pos]),
		SNewName ="EjasonVar"++integer_to_list(Count2),
 		NewName = list_to_atom(SNewName),
		FTerms = ParamsNewTerms ++ LabelNewTerms,
		Formula = jasonParser:removeParamTypes([T]),
		{Count2+1,
		 replaceTermWith(Formula, NewName,Terms)++FTerms,
		 replaceTermWith(Formula, NewName,LabelNewVars)}
 	end,
%    io:format("NewTerm: ~p~n",[NewTerms]),
%    io:format("NewVars: ~p~n",[NewVars]),

    replaceNonVarsForVars(Terms,NewVars,NewCount,AccTerms++NewTerms).


%% Given a term name and an atom, changes the name of a 
%% variable in the var list given as parameter everytime it appears.
replaceTermWith(_VarName,_Replacement,[])->
    [];
replaceTermWith(VarName, Replacement,[VarTerm|VarTerms]) ->
    %io:format("Replacing: ~p for ~p in ~p~n",[VarName,Replacement,VarTerm]),
    case VarTerm of
	VarName ->
	    [Replacement|replaceTermWith(VarName, Replacement,VarTerms)];
	{atom,Line,VarName}->
	    [{var,Line,Replacement}|
	     replaceTermWith(VarName, Replacement,VarTerms)];
	{var,Line,Atom} ->
	    [{var,Line,Atom}|
	     replaceTermWith(VarName, Replacement,VarTerms)];
	{formula,Atom,Pars,Label} ->
	    NewAtom = if
			  Atom == VarName ->
			      Replacement;
			  true ->
			      Atom
		      end,
	    NewPars = list_to_tuple(
			replaceTermWith(VarName, Replacement, 
				       Pars)),
	    
	    NewLabel = 	replaceTermWith(VarName, Replacement, Label),
	    [{NewAtom,NewPars,NewLabel}|
	     replaceTermWith(VarName, Replacement,VarTerms)];
	{Atom,Pars,Label} when is_tuple(Pars)->
	    NewAtom = if
			  Atom == VarName ->
			      Replacement;
			  true ->
			      Atom
		      end,
	    NewPars = list_to_tuple(
			replaceTermWith(VarName, Replacement, 
				       tuple_to_list(Pars))),
	    
	    NewLabel = 	replaceTermWith(VarName, Replacement, Label),
	    [{NewAtom,NewPars,NewLabel}|
	     replaceTermWith(VarName, Replacement,VarTerms)];
	_ ->
	    io:format("[utils] UNEXPECTED and unchanged ~p~n",[VarTerm]),
	    [VarTerm|replaceTermWith(VarName, Replacement,VarTerms)]
    end.



replacePositionInList([],Index,_)->
    io:format("WRONG REPLACEMENT. INDEX ~p TOO LONG~n",[Index]),
    [];
replacePositionInList([_Element|Rest],1,NewElement) ->
    [NewElement|Rest];
replacePositionInList([Element|Rest],N,NewElement) when N > 1->
    [Element|replacePositionInList(Rest,N-1,NewElement)];
replacePositionInList(List,N,_NewElement) when N < 1->
    io:format("WRONG REPLACEMENT. INDEX TOO SHORT~n"),
    List.



%% TODO: study applicability of annotations in external actions
external_action(EnvironmentName,ExAc = {#var{bind = ActName},Args,Annot})->
    Fun = fun (X) ->
		  case binds_for_vars(X) of
		      {Value,{},[]}->
			  Value;
		      Other->
			  Other
		  end
	  end,	  
    
    Params = lists:map(Fun, tuple_to_list(Args)),
    %Annotations = list_to_tuple(lists:map(Fun, Annot)),
    TimeStamp = erlang:now(), % Will serve as identifier for the 
                               % response message
    
 %   Annotations = lists:map(Fun,Annots),
   
%    io:format("[utils] EX Ac Params: ~p~n",[Params]),
   % io:format("Action: ~p:~p~n",[EnvironmentName,ActName]),
    ExecutingProcess = 
	spawn(environment_handler,
	      execute,
	      [EnvironmentName,
	       ActName,Params,Annot,self(),TimeStamp]),%% Continue
    {suspend,ExecutingProcess,TimeStamp}.



internal_action(Package, InAc = {#var{bind = ActName},Args,Annot}) ->
   % io:format("Package: ~p~n",[Package]),
   % io:format("InternalAction: ~p ~n",[InAc]),
    
 %   io:format("Action: ~p ~n~p~n",[Args,size(Args)]),
    Fun = fun (X) ->
		  binds_for_vars(X) end,	  
		  
    Params = list_to_tuple(lists:map(Fun, tuple_to_list(Args))),
 %  io:format("[utils] InAc Params: ~p~n",[Params]),
    %*****
    execute(Package,Package,Package,{ActName,Params,Annot}).

%% TODO: rename
%% 
%% Returns the binding of a variable if it is bound. 
%% Returns the variable otherwise
binds_for_vars([])->
    [];
binds_for_vars(Var = #var{bind = ?UNBOUNDVAR})->
    Var;
binds_for_vars(#var{bind = Bind})->
    Bind;
binds_for_vars({PName,PArgs,PAnnot}) ->
    Fun = fun (X) ->
		  binds_for_vars(X) end,
    {binds_for_vars(PName),
     list_to_tuple(lists:map(Fun,tuple_to_list(PArgs))),
     lists:map(Fun,PAnnot)};
binds_for_vars(List) when is_list(List)->
     Fun = fun (X) ->
		  binds_for_vars(X) end,
    lists:map(Fun,List).



%% TODO: make different cases for execute/4 (normally fewer params are needed)




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% INTERNAL ACTIONS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%% SEND a message to some other agent
execute(_Name, _Module,'.',{send,{Receiver,
				 {Intention,{},[]},Message},_})->

%% Receiver can be an agent's name or a structure  AgentName[Container]
%io:format("Receiver: ~p~n",[Receiver]),
    {AgentName,Container} = 
	case  Receiver of
	    Receiver when is_atom(Receiver) ->
		{Receiver,node()};
	      % Pid when is_pid(Pid)->
	%	  Pid; 
	    {RecvName,{},[{container,{RecvContainer},_}]}->
		RName = case RecvName of
			    _ when is_atom(RecvName)->
				RecvName;
			    {AtomRecv,{},[]} ->
				AtomRecv
			end,
		RContainer = case RecvContainer of
				 _ when is_atom(RecvContainer)->
				     RecvContainer;
				 {AtomContainer,{},[]} ->
				     AtomContainer
			     end,
		{RName,RContainer};		  
	    {Atom,{},[]} ->
		{Atom,node()}
	end,

    NewIntention = % performative
	case Intention of
	       _ when is_atom(Intention) ->
		   Intention;
	       {AtomInt,{},[]} ->
		   AtomInt
	   end,

    %io:format("~p receiver is: {~p,~p}~n",[erlang:now(),ReceiverID,node()]),

    %io:format("{Intention,Message} is: {~p,~p}~n",[NewIntention,Message]
    
    case process_info(self(),registered_name) of
	    {registered_name,MyName}->
	    FindAgentID = ?DM:find_agent(AgentName),
	    ID = {?DM,FindAgentID},
	    SuspendedAction = 
		#suspendedSend{
	      my_name = MyName,
	      performative = NewIntention,
	      message = Message
	     },
	    {?ACTIONSUSPEND,ID,SuspendedAction};
	    	   
	    _ ->
		io:format("[Utils] Unregistered Agent sends~n"),
	        exit(not_registered_agent),
		{?FAIL}
	end;






%% Connect to a container
execute(Name, _Module,'.',{connect,{Container},_Label})->
    ContainerName = 
	case Container of
	    _ when is_atom(Container) ->
		Container;
	    {ContainerAtom,{},[]} ->
		ContainerAtom
	end,
    %io:format("[~p]connection requested ~n",[self()]),
    ConnectID =
	?DM:connect(ContainerName),
    ID = {?DM,ConnectID},
    SuspendedAction = 
	#suspendedConnect{
      container = ContainerName
     },
    {?ACTIONSUSPEND,ID,SuspendedAction};
	    

    %% case Return of
    %% 	{?STUTTERACTION} ->
    %% 	    io:format("Connected to ~p~n",[ContainerName]);
    %% 	_ ->
    %% 	    ok
    %% end,
         
    %% Return;


%% Disconnect from a container
execute(Name, _Module,'.',{disconnect,{Container},_Label})->
    ContainerName = 
	case Container of
	    _ when is_atom(Container) ->
		Container;
	    {ContainerAtom,{},[]} ->
		ContainerAtom
	end,
    %io:format("[~p]connection requested ~n",[self()]),

    DisconnectID =
	?DM:disconnect(ContainerName),
    ID = {?DM,DisconnectID},
    SuspendedAction = 
	#suspendedDisconnect{
      container = ContainerName
     },
    {?ACTIONSUSPEND,ID,SuspendedAction};

%% MAKE THE PLAN FAIL
execute(_Name, _Module,'.',{fail,_PARAMS,_Label})->
    {?FAIL};


%% CREATE a new agent
%% execute(Name, Module,'.',{create_agent,{AName,
%% 					 ParamCode},Label})->
%%     execute(Name, Module, '.', {create_agent,
%% 				{AName,node(,
%% 				 ParamCode,[]},Label});	


execute(Name, Module,'.',{create_agent,{AName,
					 ParamCode},Label})->
    execute(Name, Module, '.', {create_agent,
				{AName,
				 ParamCode,[]},Label});	

execute(_Name, _Module,'.',{create_agent,{AName,ParamCode,_Custom},_Label})->
%    io:format("AName: ~p~n",[AName]),
    {AgentName,Node} = 
		case AName of
		    _ when is_atom(AName) ->
			{AName,node()};
		    {AtomName,{},[]} ->
			{AtomName,node()};
		    {AtomName,{},[{container,{ContainerName},_}]} ->
			%io:format("ContainerName: ~p~n",
			%	  [ContainerName]),
			    case ContainerName of
				_ when is_atom(ContainerName) ->
				    {AtomName,ContainerName};
				{AtomContainer,{},[]} ->
				    {AtomName,AtomContainer}
			    end
		end,
   %   io:format("ParamCode: ~p~n",[ParamCode]),
% io:format("InputNode: ~p~n",[InputNode]),
 %io:format("New2!!~n~n"),
  Code = 
	case ParamCode of
	    _ when is_list(ParamCode) ->
		list_to_atom(string:sub_string(ParamCode,1, 
					       length(ParamCode) -4));
	    {AtomCode,{},[]} when is_atom(AtomCode) ->
		AtomCode;
	    {StringCode,{},[]} when is_list(StringCode)->
		list_to_atom(string:sub_string(StringCode,1, 
					       length(StringCode) -4));
	    _ when is_atom(ParamCode) ->
		ParamCode	    
	end,

%% TODO: check whether code can be found!!
 

   CreateID =
	?DM:create_agent(AgentName,Node,Code),
     ID = {?DM,CreateID},
    SuspendedAction = 
	#suspendedCreateAgent{
      agent_name = AgentName,
      container = Node,
      code = Code
     },
    {?ACTIONSUSPEND,ID,SuspendedAction};
 
   	
    %% 	case Return of
    %% 	    {?STUTTERACTION} ->
    %% 		io:format("Agent ~p created.~n",[AgentName]);
    %% 	    _ ->
    %% 		io:format("Agent ~p not created.~n",[AgentName])
    %% 	end,
    %% Return;

%*****



%% PRINT some string in the standard output
execute(_Name,_module,'.',{print,String,_})-> 
   % io:format("IMPRIMIENDO ~p~n",[String]),
    NewString =     print_list(tuple_to_list(String)),
    MyName = case process_info(self(),registered_name) of
		 [] ->
		     io:format("[Utils] Unregistered agent~n"),
		     unreg_agent;
	%		 exit(fatal_error);
		 {registered_name,AgentName}->
		     AgentName
	     end,
%     io:format("[~p:~p]: ~p~n",[MyName,node(),NewString]),
   io:format("[~p]: ~s",[MyName,NewString]),
%    io:format("[~p,~p]: ~s",[Name,erlang:now(),NewString]),

   {?STUTTERACTION};

execute(_Name,_Module,'.', {to_string,{Value,Var},_}) when is_record(Var,var)->
    try
      String = 
	case Value of
	    _ when is_atom(Value)->
		atom_to_list(Value);
	    _ when is_float(Value) ->
		float_to_list(Value);
	    _ when is_integer(Value)->
		integer_to_list(Value);
	    _ when is_boolean(Value)->
		if 
		    Value ->
			"true";
		    true ->
			"false"
		end;
	    _ when is_list(Value)->
		true = is_string(Value), 
		Value
	end,
		    
	{update_bindings,[Var#var{bind = String}]}

    catch
	_:_->
	    {fail}
    end;  

execute(_Name,_Module,'.', {to_number,{Value,Var},_}) when is_record(Var,var)->
    try
      Number = 
	case Value of
	    _ when is_list(Value)->
		try
		    list_to_integer(Value)
		catch
		    _:_->
			list_to_float(Value)
		end;
	    _ when is_float(Value)->
		Value;
	    _ when is_integer(Value)->
		Value	    
	end,
	{update_bindings,[Var#var{bind = Number}]}

    catch
	_:_->
	    {fail}
    end;  



%% KILL another agent (the process running it)
execute(Name,_Module,'.',{kill_agent,{AName},_Annot}) ->
   
%% AName can be an agent's name or a structure AgentName[Container]
%    io:format("Killing: ~p~n",[AName]),
    AgentName = 
	case AName of
	    _ when is_atom(AName) ->
		AName;

	    %% case whereis(AName) of
	   %% 	Pid when is_pid(Pid) ->
	   %% 	    exit(Pid,kill) ;
	   %% 	_->
	   %% 	    ok
	   %%  end;
	{AtomName,{},[]} ->
	   AtomName
		%% case whereis(AtomName) of
	    %% 	Pid when is_pid(Pid) ->
	    %% 	    exit(Pid,kill) ;
	    %% 	_->
	    %% 	    ok
    
    end,
    KillID = ?DM:find_agent(AgentName),
    ID = {?DM,KillID},
    SuspendedAction = 
	#suspendedKillAgent{
      agent_name = AgentName},
    {?ACTIONSUSPEND,ID,SuspendedAction};

%% execute(_Name,_Module,'.',{kill_agent,{AName,
%% 				      Container},_Annot}) ->

%% %    io:format("Killing: ~p in ~p~n",[AName,Container]),

%%     AgentName = 
%% 		case AName of
%% 		    _ when is_atom(AName) ->
%% 			AName;
%% 		    {AtomName,{},[]} ->
%% 			AtomName
%% 		end,


%%     Node =
%% 	case Container of
%% 	    _ when is_atom(Container) ->
%% 		Container;
%% 	    {AtomNode,{},[]} ->
%% 		AtomNode
%% 	end,


%%     Mypid = self(), 
%%     Fun = fun () -> W = whereis(AgentName), Mypid ! {pid_request,W} end,
%%     spawn(Node,Fun),
%%     receive 
%% 	{pid_request,PidToKill} ->
%% 	    exit(PidToKill,kill)
%%     after 
%% 	1000 ->
%% 	ok
%%     end,
%%     {?STUTTERACTION};



%% WAIT: agent sleeps during WaitTime milliseconds

execute(Name,_Module,'.',{wait,{WaitTime},_Annot}) ->
 

    case  WaitTime of
	
	_ when is_integer(WaitTime) ->
	    timer:sleep(WaitTime),
	    {?STUTTERACTION};
	
	{Time,{},[]} when is_integer(Time)->
	    timer:sleep(Time),
	    {?STUTTERACTION};	
	_->
	    {fail, io_lib:format("WaitTime in .wait is: ~p",[WaitTime])}
    end;


%% MONITOR another agent (the process running it)

execute(Name,_Module,'.',{monitor,{AID},_Annot}) ->
 
    try 
	RegName = 
	case  AID of
	   	%	Pid when is_pid(Pid) ->
%	    monitor(process,Pid),
%	    {?STUTTERACTION};
	
	     
 
	    _ when is_atom(AID) ->
	       %  monitor(process,RegName),
		AID;
	    %monitor(process,{RegName,node()}),
	    %{?STUTTERACTION};
	    {#var{bind = SomeName},_,_}->
		   SomeName;
	    


	    {AtomName,{},[]} when is_atom(AtomName)->
	      %	    monitor(process,RegName),
		AtomName
	end,
    {_,MyName} = process_info(self(),registered_name),

%io:format("~p monitors ~p ~n",[MyName, RegName]),

    ?SM:monitor(MyName, RegName)
    catch
	_:_ ->
	    {?FAIL}
    end;

%	    io:format("~p monitors ~p (~p) ~p~n",[self(), RegName, 
%						  whereis(RegName), 
%						  monitor(process,RegName)]),
    	    %monitor(process,{RegName,node()}),

%	    {?STUTTERACTION};	

%% 		  {RegName,{},[{container,{Container},_}]}->
%% 	    execute(Name,"",'.',{monitor,{RegName,Container},[]});  
%% %    	    monitor(process,{RegName,node()}),



%	    {?STUTTERACTION};
	%_->
	%    {?FAIL}


%% execute(Name,_Module,'.',{monitor,{AName,
%% 				   Arch},_Annot}) ->

%%    RegName = case AName of
%% 	       _ when is_atom(AName) ->
%% 		   AName;
%% 	       {AtomName,{},[]} when is_atom(AtomName)->
%% 		   AtomName
%% 	   end,

%%     Node =
%% 	case Arch of
%% 	    _ when is_atom(Arch) ->
%% 		Arch;
%% 	    {AtomNode,{},[]} ->
%% 		AtomNode
%% 	end,
    
    
%%     case {RegName,Node} of
%% 	AID when is_atom(RegName), is_atom(Node) ->
%% 	    monitor(process,AID),
%% 	    {?STUTTERACTION};
	
%% 	_->
%% 	    {?FAIL}
%%     end;


%% DEMONITOR another agent (the process running it)

execute(Name,_Module,'.',{demonitor,{AID},_Annot}) ->
 
    try 
	RegName = 
	case  AID of
	    _ when is_atom(AID) ->
	       %  monitor(process,RegName),
		AID;

	    {#var{bind = SomeName},_,_}->
		   SomeName;
	    


	    {AtomName,{},[]} when is_atom(AtomName)->
	      %	    monitor(process,RegName),
		AtomName
	end,
    {_,MyName} = process_info(self(),registered_name),

%io:format("~p demonitors ~p ~n",[MyName, RegName]),

    ?SM:demonitor(MyName, RegName)
    catch
	_:_ ->
	    {?FAIL}
    end;



%% GET AID by returning the pid of the erlang process running it
execute(_Name,_Module,'.',NameQuery = {my_name,{Variable},_Annot}) -> %when   is_record(Variable,var)->

    add_ejason_private_query(NameQuery);


%% GET Architecture name by returning the name of the node the agent runs on
execute(_Name,_Module,'.',ArchQuery = {my_container,{Variable},_Annot}) when 
  is_record(Variable,var)->
    add_ejason_private_query(ArchQuery);

%% GET The environment file that interfaces to the set of external actions
execute(Name,_Module,'.',ArchQuery = {my_environment,{Variable},_Annot}) when 
  is_record(Variable,var)->
    add_ejason_private_query(ArchQuery);

execute(_Name,_Module,'.',Action) ->
    io:format("Undefined internal action: ~p~n",[Action]),
    {fail};
execute(_Name,_Module,Package,Action) ->
   % io:format("Undefined Action: ~p in Package ~p~n",[Action,Package]),
    case 
	environment_handler:execute_internal(Package,Action) of
	{fail} ->
	    {fail};
	{ok,NewBindings} ->
	    {update_bindings,NewBindings}
    end.




print_list([])->
    "\n";
print_list(List)->
%    io:format("List: ~p~n",[List]),
    Fun = fun (X) ->print_elem(X) end,
    lists:flatten(lists:map(Fun,List))++"\n".
    
    


print_elem({PName,PArgs,PAnnot}) when is_tuple(PArgs),
				     is_list(PAnnot)->
 %   io:format("Name: ~p~nArgs: ~p~nAnnot: ~p~n",
 %     [PName,PArgs,PAnnot]),
    Fun = fun (X) ->
		  print_elem(X) end,
    SName = print_elem(PName),
    SPred=
	case PArgs of
	    {}->
		io_lib:format("~s",
			      [SName]);
	    _ ->
		io_lib:format("~s(~s)",
			  [SName,
			   string:join(
			     lists:map(Fun,tuple_to_list(PArgs)),", ")])
	end,
    case PAnnot of
	[]->
	       SPred;
	_ ->
	    io_lib:format("~s[~s]",
			  [SPred,
			   string:join(
			     lists:map(Fun,PAnnot),", ")])
    end;

print_elem(#var{bind = {}, name = Name})->% Unbound var
    atom_to_list(Name) ++ "<no value>";
print_elem(Else) ->
%  io:format("Else: ~p~n",[Else]),
   String = if
		is_integer(Else)->
		    integer_to_list(Else);
		is_atom(Else)->
		    atom_to_list(Else);
		is_list(Else) ->
		    case is_string(Else) of
			true->
			    Else;
			false ->
			    string:join(lists:map(fun print_elem/1 ,Else),"")
		    end;
		is_pid(Else)->
		    pid_to_list(Else);
		is_record(Else,var)->
		    print_elem(Else#var.bind);
		true->
		    io:format("[Utils] Error in to_string(~p)\n",[Else]),
		    exit(error_in_utils)
	    end,
    String.

%% Boolean function that returns true if the term given is a list of
%% integers.
is_string([])->		  
    true;
is_string([Int|Rest]) when is_integer(Int)->
    is_string(Rest);
is_string(_) ->
    false.


%% ;
%% execute(Name,_module,'.',{print,{String},_}) when is_atom(String)-> 
%%     %Now only prints one string at a time
%%     io:format("[~p]: ~p~n",[Name,String]).


%% register_agent(OrderNum,Pid,Name,Uniqueness)->
%% %   io:format("Name: ~p~nNum: ~p~n",[Name,OrderNum]),
%%     NewName = case(OrderNum) of
%% 		  1 ->
%% 		      Name;
%% 		  _ ->
%% 		      erlang:list_to_atom(
%% 			lists:flatten(
%% 			  io_lib:format("~p_~p",[Name,OrderNum])))
%% 	      end,
%%     case whereis(NewName) of
%% 	undefined->
%% 	    register(NewName,Pid),
%% 	    NewName;
%% 	_ ->
%% 	    case Uniqueness of
%% 		?UNIQUE ->
%% 		    NewName;
%% 		    %exit(already_existing_agent);
%% 		?NOTUNIQUE->
%% 		    %NewName = erlang:list_to_atom(
%% 		%		lists:flatten(
%% 		%		  io_lib:format("~p_~p",[Name,OrderNum]))),
%% 		    utils:register_agent(OrderNum+1,Pid,Name,Uniqueness)
%% 	    end
%%     end.
	    

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
	


resolve_test_goal(Q = {_Atom,Terms,_Label},
		   Module,KB,FunNames)->
   %% io:format("Test goal for query: ~p~n",[Q]),
    query_bb(Module,KB,Q,[Terms],FunNames);
resolve_test_goal({Atom},
		   Module,KB,FunNames) ->
    query_bb(Module,KB,{Atom},[],FunNames).



%% Replaces the var timestamps given, with the proper
%% variables in the argument "Bindings"
valuate(Bindings,TS = {Sec,Msec,Nsec})when is_integer(Sec),
                                      is_integer(Msec),
                                      is_integer(Nsec) ->

    Var = variables:find(Bindings,TS),
   % io:format("Found: ~p~n",[Var]),
    case Var of
	#var{bind = Bind} when is_tuple(Bind),size(Bind) > 0->
%	    io:format("Bind: ~p~n",[Bind]),
	     Var#var{bind=valuate(Bindings,Bind)};
	_ ->
	    Var
    end;
%    valuate(Bindings,Vars);
%    valuate(Bindings,variables:find(Bindings,TS));
valuate(_Bindings,Var) when is_record(Var,var) ->
  %      io:format("Valuate Var: ~p~n",[Var]),

    Var;
valuate(Bindings,{PredName, Args,Annot}= Pred) when is_tuple(PredName),
					      is_tuple(Args),
					      is_list(Annot)
					      ->
   Val =  {valuate(Bindings,PredName),
    list_to_tuple(valuate(Bindings,tuple_to_list(Args))),
     valuate(Bindings,Annot)},
%    io:format("We valuate Pred: ~p~nas ~p~n",[Pred,Val]),
    Val;

valuate(Bindings,List) when is_list(List)->
  %      io:format("Valuate List: ~p~n",[List]),

    Fun = fun (X) ->
		  valuate(Bindings,X) end,
    lists:map(Fun,List);
valuate(Bindings, BO = #binary_operation{
		    left_part = [Left],
		    right_part = [Right]})->
    %io:format("BO: ~p~n",[BO]),
   Res =  BO#binary_operation{
      left_part = [valuate(Bindings,Left)],
      right_part = [valuate(Bindings,Right)]},
   % io:format("ResBO: ~p~n",[Res]),
    Res;
valuate(A,B) ->
 %   io:format("[utils]cannot valuate-> A: ~p~nB: ~p~n",[A,B]),
    B. 



%% Constructs the tuple {name, {args()},[annot()]} equivalent to
%% the given predicate.
%predicate_to_tuple(#predicate{name = Name,
%			      arguments ={},
%			      annotations = _Annot})->
 %   valuate_arg(Name);
predicate_to_tuple(#predicate{name = Name,
			     arguments = Args,
			     annotations = Annot}) ->
    TName = Name#var.bind,
    TArgs = lists:map(fun ?MODULE:valuate_arg/1,tuple_to_list(Args)),
    TAnnot = lists:map(fun ?MODULE:predicate_to_tuple/1,
		       Annot),
    {TName,list_to_tuple(TArgs),TAnnot}.
    


valuate_arg(Var) when is_record(Var,var)->
    var_to_bind(Var);
valuate_arg(Predicate) when is_record(Predicate,predicate)->
    predicate_to_tuple(Predicate).

var_to_bind(#var{bind = Pred}) when is_record(Pred,predicate)->
    predicate_to_tuple(Pred);
var_to_bind(#var{bind = Bind})->
    Bind.


%% Obtains a list of all distincs variables in the parameter
gather_all_distinct_vars(Terms)->
    RepeatedRes = gather_all_vars(Terms),
    erase_repeated_vars(RepeatedRes).


gather_all_vars(Var = #var{}) ->
    Var;
gather_all_vars(#predicate{name = Name,
			  arguments = Args,
			   annotations = Annot}) ->
    Fun = fun (X) ->
		  gather_all_vars(X) end,
    lists:flatten(
      lists:map(Fun,[Name|tuple_to_list(Args)]++
		Annot));
gather_all_vars(List) when is_list(List) ->
    Fun = fun (X) ->
		  gather_all_vars(X) end,
   lists:flatten(lists:map(Fun,List)).




erase_repeated_vars(VarList)->
    erase_repeated_vars(VarList,[]).

erase_repeated_vars([],Acc)->
    Acc;
erase_repeated_vars([Var|VarList],Acc)->
    case is_var_in_list(Var,VarList) of
	true ->
	    erase_repeated_vars(VarList,Acc);
	false ->
	    erase_repeated_vars(VarList,[Var|Acc])
	    
    end.

is_var_in_list(_,
	       [])->
    false;
is_var_in_list(#var{timestamp = TS1},
	       [#var{timestamp = TS1}|
	       _List]) ->
    true;
is_var_in_list(Var,[_|List]) ->
    is_var_in_list(Var,List).

%make_tuple(#var{bind = Bind})


% Replaces all variables for the term they are bound to
%% Note: used to generate a belief from an event.
vars_to_bindings(Atom) when is_atom(Atom) ->
    Atom;
vars_to_bindings(Integer) when is_integer(Integer) ->
    Integer;
vars_to_bindings(Var =#var{timestamp = TS,bind = ?UNBOUNDVAR}) ->
    %io:format("[utils] Returning timestamp as bind for var: ~p~n",[Var]),
    TS;
vars_to_bindings(#var{bind = Bind}) ->
    vars_to_bindings(Bind);
vars_to_bindings(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(vars_to_bindings(tuple_to_list(Tuple)));
vars_to_bindings(List) when is_list(List)->
    Fun = fun (X) ->
		  vars_to_bindings(X) end,
    lists:map(Fun,List).


%% Turns a predicate into a three-element tuple with
%% vars in EACH position
%% Used to add the initial beliefs
predicate_to_vars_tuple(#predicate{name = Name,
				  arguments = Args,
				  annotations = Annot}) ->
    TupleName = 
	case Name of
	    #var{}->
		Name;
	    Name when is_atom(Name) ->
		#var{is_ground= true,
		     bind = Name}
	end,

    Fun = fun (X) -> elem_to_var(X) end,
    TupleArgs = 
	list_to_tuple(lists:map(Fun,
				tuple_to_list(Args))),
   
    TupleAnnot = 
	lists:map(Fun,Annot),
    {TupleName,TupleArgs,TupleAnnot}.
	


elem_to_var(Var) when is_record(Var,var) ->
    Var;
elem_to_var(Pred) when is_record(Pred,predicate) ->
    predicate_to_vars_tuple(Pred);
elem_to_var(Elem) ->
    	#var{is_ground= true,
		     bind = Elem}.


%% Turns a predicate into a three-element tuple with
%% timestamps in EACH position

predicate_to_timestamp_tuple(#predicate{name = Name,
					arguments = Args,
					annotations = Annot}) ->
    TupleName = Name#var.timestamp,
    
    Fun = fun (X) -> elem_to_timestamp(X) end,
    TupleArgs = 
	list_to_tuple(lists:map(Fun,
				tuple_to_list(Args))),
    
    TupleAnnot = 
	lists:map(Fun,Annot),
    {TupleName,TupleArgs,TupleAnnot}.


elem_to_timestamp(#var{timestamp = TS}) ->
    TS;
elem_to_timestamp(Pred) when is_record(Pred,predicate) ->
    predicate_to_timestamp_tuple(Pred).



%%%%%%%%%%%%%%%% OPERATIONS CALLED IN A PLAN BODY

operation(Type,Arg1,Arg2)->
    try
	NewVar = utils:Type(Arg1,Arg2),
	{update_bindings,[NewVar]}
    catch
	exit:{unbound_var,VarName}->
	    io:format("Var ~p is unbound. Plan fails.\n",[VarName]),
	    {fail}    
    end.


%%%%%%%%%%%%%%%% ARITHMETIC

arith_plus(#var{bind = Bind1},#var{bind = Bind2})->
    #var{is_ground = true, bind = Bind1 +  Bind2}.

arith_minus(#var{bind = Bind1},#var{bind = Bind2})->
    #var{is_ground = true, bind = Bind1 -  Bind2}.

arith_mult(#var{bind = Bind1},#var{bind = Bind2})->
    #var{is_ground = true, bind = Bind1 *  Bind2}.

arith_power(#var{bind = Bind1},#var{bind = Bind2})->
    #var{is_ground = true, bind = math:pow(Bind1,Bind2)}.

arith_slash(#var{bind = Bind1},#var{bind = Bind2})->
    #var{is_ground = true, bind = Bind1 /  Bind2}.

arith_div(#var{bind = Bind1},#var{bind = Bind2})->
    #var{is_ground = true, bind = (Bind1 div Bind2)}.

arith_mod(#var{bind = Bind1},#var{bind = Bind2})->
    #var{is_ground = true, bind = (Bind1 rem  Bind2)}.



%%%%%%%%%%%%%%%% Relative expressions

rel_ge(#var{bind = Bind1},#var{bind = Bind2})->
    Bind1 >= Bind2.


rel_lt(#var{bind = Bind1},#var{bind = Bind2})->
    Bind1 < Bind2.

rel_le(#var{bind = Bind1},#var{bind = Bind2})->
    Bind1 =< Bind2.

rel_gt(#var{bind = Bind1},#var{bind = Bind2})->
    %io:format("Bind1: ~p~nBind2: ~p~n",[Bind1,Bind2]),
    Bind1 > Bind2.

rel_eq(#var{bind = Bind1},#var{bind = Bind2})->
    Bind1 == Bind2.

rel_diff(#var{bind = Bind1},#var{bind = Bind2})->
    Bind1 /= Bind2.

rel_assig(Var1,#var{bind = Bind2})->
    Var1#var{bind = Bind2};
rel_assig(Var1,BO= #binary_operation{operator = OP,
				left_part = [Left],
				right_part = [Right]}) ->
 %  io:format("Var1: ~p~nBO: ~p~n",[Var1,BO]),
    Var1#var{bind = utils:OP(Left,Right)}.
 


rel_decomp({Name,Args,Annot})->
     #var{bind = [Name,tuple_to_list(Args),Annot]}.

%%%%%%%%%%%%%%%% Logical expressions
log_not(#var{bind = Bind})when is_boolean(Bind)->
    not Bind;
log_not(Bool) when is_boolean(Bool)->
    not Bool;
log_not(_) ->
    false.
