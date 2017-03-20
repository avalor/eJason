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

-include("include/variables.hrl").
-include("include/macros.hrl").
-include("include/ejason.hrl").



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



%% Generates an event ?ADDBELIEF labelled with source(self)
add_belief(Bindings,Belief) ->
    Now = variables:make_timestamp_string(),
    add_belief(Bindings, Belief, Now, self).

%%     {CorrectedBindings,CorrectedBelief} = 
%% 	variables:correct_structs(
%% 	  Bindings,
%% 	  Belief),
    
%%     NewBelief =
%% 	variables:valuate(CorrectedBindings,CorrectedBelief),


%%     %% Add annotation [source(self)]

%%     SourceStructID = 
%% 	list_to_atom(
%% 	  "EJASONSOURCEBEL"++variables:make_timestamp_string()),

%%     SelfVar =
%% 	#var{id =self,
%% 	     functor = self,
%% 	     args = ?ISATOM},

%%     SourceVar =
%% 	#var{id =source,
%% 	     functor = source,
%% 	     args = ?ISATOM},
    
%%     SourceStructVar =
%% 	#var{id = SourceStructID,
%% 	     functor = {source},
%% 	     args = {{self}},
%% 	     annots = []},
    
%%     SourceBindings =
%% 	variables:update(
%% 	  CorrectedBindings,
%% 	  [SelfVar,SourceVar,SourceStructVar]),
    

%%     ValuatedSource =
%% 	variables:valuate(SourceBindings,
%% 			  SourceStructVar),
       
%%     BeliefWithSource =	      
%% 	NewBelief#var{annots =
%% 		      %% Added at the end
%% 		      lists:reverse([ValuatedSource|
%% 				     lists:reverse(NewBelief#var.annots)])},


%% %%   io:format("Belief added: ~p~n",[BeliefWithSource]),
%%     #event{type = ?ADDBELIEF,
%% 	   body = BeliefWithSource}.


%% Generates an event ?ADDBELIEF
add_belief(Bindings,NotValuatedBelief,Timestamp,Sender) ->  

%% Added in case Belief is a reference to another belief
    Belief =
	variables:valuate(Bindings, NotValuatedBelief),


    %% io:format("[utils] AddBelief: ~p~n",[Belief]),
    %% io:format("[utils] NotValuatedBelief: ~p~n",[NotValuatedBelief]),


%%% Add source(Sender) which may be "percept" or "self"
    SenderVar =
	#var{id = Sender,
	     functor = Sender,
	     args = ?ISATOM},

    SourceVar =
	#var{id =source,
	     functor = source,
	     args = ?ISATOM},
    
    SourceStructVar =
	#var{id = list_to_atom("VARFORNEWBELIEF"++Timestamp++"_1"),
	     functor = SourceVar,
	     args = {SenderVar},
	     annots = []},
    
    
    BeliefWithSource =
	case Belief of 
	    %% Content is an atom. e.g. a,b (not number or string) 
	    #var{args = ?ISATOM, functor = SomeAtom} 
	      when is_atom(SomeAtom)->
		#var{ id = list_to_atom("VARFORNEWBEL"++Timestamp++"_0"),
		      functor = Belief,
		      args = {},
		      annots = [SourceStructVar]};
	    %%Content is a strong negation
	    #var{args = ?STRONGNEG,
		 annots = []} ->
		Belief#var{annots = [SourceStructVar]};
	    

	    %% Content is a struct
	    #var{args = Args,
		 annots = Annots} when is_tuple(Args)->
		
		%% annot. source(Sender) is put at the end
		Belief#var{
		  annots = lists:reverse(
			     [SourceStructVar|
			      lists:reverse(Annots)])};
	    _ ->
		%%% The belief cannot be added. e.g: +[1,2,3]; 
		{?FAIL}
	end,

    case BeliefWithSource of
	{?FAIL} ->
	    {?FAIL};

	_ ->
	    %% Belief correction is carried out. Added lately, so
	    %% check for dependencies.
    
	    {CorrectedBindings,CorrectedBelief} = 
		variables:correct_structs(
		  Bindings,
		  BeliefWithSource),
	    
	    NewBelief =
		variables:valuate(CorrectedBindings,CorrectedBelief),


	    #event{type = ?ADDBELIEF,
		   body = NewBelief}
    end.





remove_belief(Bindings,Belief)->
    {CorrectedBindings,CorrectedBelief} = 
	variables:correct_structs(Bindings,Belief),
    %% io:format("CorrectedGoal: ~p~n",[CorrectedGoal]),

    %%TODO: remove Belief[source(self)]
    NewBelief =
	variables:valuate(CorrectedBindings,CorrectedBelief),


   %io:format("UTILS: Remove Belief: ~p~n",[Belief]),
     #event{type = ?REMOVEBELIEF,
	   body = NewBelief}.


%% -+Belief
remove_add_belief(Bindings,Belief)->
    {CorrectedBindings,CorrectedBelief} = 
	variables:correct_structs(Bindings,Belief),
    %% io:format("CorrectedGoal: ~p~n",[CorrectedGoal]),

    NewBelief =
	variables:valuate(CorrectedBindings,CorrectedBelief),


    #event{type = ?REMOVEADDBELIEF,
	   body = NewBelief}. 

%% !!Goal
new_intention_goal(Bindings,Goal)->
    {CorrectedBindings,CorrectedGoal} = 
	variables:correct_structs(Bindings,Goal),
    %% io:format("CorrectedGoal: ~p~n",[CorrectedGoal]),
    NewGoal =
	variables:valuate(CorrectedBindings,CorrectedGoal),

    GoalCheck =
	test_goal_format(NewGoal),

    case GoalCheck of 
	true ->
	    #event{type = ?ADDINTENTIONGOAL,
		   body = NewGoal};
	false ->
	    {?FAIL}
    end.




%% !Goal[source(self)]
add_achievement_goal(Bindings,Goal)->
    {CorrectedBindings,CorrectedGoal} = 
	variables:correct_structs(Bindings,Goal),
     %% io:format("utils  CorrectedGoal: ~p~n",[CorrectedGoal]),
    %% io:format("Bindings: ~p~n",[Bindings]),


    %% Check whether the goal is bad formed:
    GoalCheck =
	test_goal_format(CorrectedGoal),

    case GoalCheck of 
	true ->
	    
	    NewGoal =
		variables:valuate(CorrectedBindings,CorrectedGoal),
	    
	    
	    SourceStructID = 
		list_to_atom(
		  "EJASONSOURCEACH"++variables:make_timestamp_string()),

    
	    SelfVar =
		#var{id =self,
		     functor = self,
		     args = ?ISATOM},
	    
	    SourceVar =
		#var{id =source,
		     functor = source,
		     args = ?ISATOM},
	    
	    SourceStructVar =
		#var{id = SourceStructID,
		     functor = SourceVar,
		     args = {SelfVar},
		     annots = []},
	    
	    GoalWithSource =	      
		NewGoal#var{annots =
				%% Added at the end
				lists:reverse(
				  [SourceStructVar|
				   lists:reverse(NewGoal#var.annots)])},
	    
	    #event{type = ?ADDACHGOAL,
		   body = GoalWithSource,
		   corrected_bindings = variables:update(CorrectedBindings,
							 [SelfVar,SourceVar,
							  SourceStructVar#var{
							    functor = {source},
							    args = {{self}}}])
		  };
	false ->
	    {?FAIL}
    end.



%% !Goal[source(Sender)]
%% Goal is already fully valuated
add_achievement_goal(_Bindings,Goal,Timestamp,Sender)->
    %% io:format("CorrectedGoal: ~p~n",[CorrectedGoal]),

   GoalCheck =
	test_goal_format(Goal),
    
    case GoalCheck of
	true ->
	    SourceStructID = 
		list_to_atom(
		  "VARFROMACHIEVE"++Timestamp++"_1"),
	    
    
	    SenderVar =
		#var{id =Sender,
		     functor = Sender,
		     args = ?ISATOM},
	    
	    SourceVar =
		#var{id =source,
		     functor = source,
		     args = ?ISATOM},
	    
	    SourceStructVar =
		#var{id = SourceStructID,
		     functor = SourceVar,
		     args = {SenderVar},
		     annots = []},
	    
	    GoalWithSource =	      
		Goal#var{annots =
			     %% Added at the end
			     lists:reverse([SourceStructVar|
					    lists:reverse(Goal#var.annots)])},
	    
	    #event{type = ?ADDACHGOAL,
		   body = GoalWithSource};
	false ->
	    {?FAIL}
    end.



add_wait_test_goal(Bindings,Goal)->
       {CorrectedBindings,CorrectedGoal} = 
	variables:correct_structs(Bindings,Goal),
    %% io:format("CorrectedGoal: ~p~n",[CorrectedGoal]),

    NewGoal =
	variables:valuate(CorrectedBindings,CorrectedGoal),

    GoalCheck =
	test_goal_format(NewGoal),
   
    case GoalCheck of
	true ->

	    #event{type = ?ADDWAITTESTGOAL,
		   body = NewGoal,
		   corrected_bindings = CorrectedBindings};
	false ->
	    {?FAIL}
    end.


add_test_goal(Bindings,Goal)->
       {CorrectedBindings,CorrectedGoal} = 
	variables:correct_structs(Bindings,Goal),
    %% io:format("CorrectedGoal: ~p~n",[CorrectedGoal]),

    NewGoal =
	variables:valuate(CorrectedBindings,CorrectedGoal),

   GoalCheck =
	test_goal_format(NewGoal),
    
    case GoalCheck of 
	true ->

	    #event{type = ?ADDTESTGOAL,
		   body = NewGoal,
		   corrected_bindings = CorrectedBindings};
	false ->
	    {?FAIL}
    end.

 



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






%% Terms: a list of term tuples (often from params)
%% Vars: a list of Varnames
%% Changes all nonvar terms ({atom,line,name}) with a new 
%% Varname ({var,line,newname} and updates
%% the list of varnames.
%% Returns a tuple {NewTerms, NewVarNames}
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

		%%		[Pos] = 
		%%		    jasonParser:getIndexesForVars(
		%%		      jasonParser:removeParamTypes([T]),
		%%						      Vars),
		%%		io:format("Pos is ~p~n",[Pos]),
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



%% Check whether an achievement/test goal is bad formed
%% It returns "true" if the goal is well formed (an atom or struct).
%% Used by add_achievement_goal and add_test_goal
test_goal_format(Goal) ->
    
    Result =
	case Goal of
	    %% Goal is an atom. e.g. a,b (not number or string) 
	    #var{args = ?ISATOM, functor = SomeAtom} when is_atom(SomeAtom)->
		true;
	    %%Goal is a strong negation
	    #var{args = ?STRONGNEG} ->
		true;
	    %% Goal is a struct
	    #var{args = Args,
		 annots = Annots} when is_tuple(Args)->
		true;
	    _ ->
		%% The goal is bad formed e.g: ![1,2,3] or ?2. 
		
		false
	end,
    %% io:format("[utils] Goal is: ~p~nResult is: ~p~n",
    %% 	      [Goal, Result]),
    Result.


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
    actions:query_bb(Module,KB,Q,[Terms],FunNames);
resolve_test_goal({Atom},
		   Module,KB,FunNames) ->
    actions:query_bb(Module,KB,{Atom},[],FunNames).


%% Returns {Path, codeName} from "Path/codeName.asl" or "Path/codeName"
split_path(String) ->
    
    case string:rstr(String,"/") of
	0 ->
	    %% Agent name without path
	    Reverse = lists:reverse(String),
	    FileName =
		case string:str(Reverse,"lsa.") of
		    1 ->
			lists:reverse(
			  string:substr(Reverse,5));
		    _ ->
			String
		end,  

	    {"",FileName};
	Num ->
	    File = string:sub_string(String, Num+1),
	    
	    Reverse = lists:reverse(File),
	    FileName =
		case string:str(Reverse,"lsa.") of
		    1 ->
			lists:reverse(
			  string:substr(Reverse,5));
		    _ ->
			File
		end,  

	    Path = ""++string:substr(String,1,
				     Num),
	    {Path,FileName}
    end.



%% Generates a random agent name 
%% Used when .create_agent receives an unbound variable
create_unique_name()->
    TS = variables:make_timestamp_string(),
    Node = erlang:atom_to_list(node()),
    erlang:list_to_atom(TS++":"++Node).



%% Deprecated. This does not avoid the collision with agent names in 
%% different containers 
create_random_name()->
    %% 1) Create the random number generator (using timestamps as seed)
    {H,M,S} = erlang:timestamp(),
    random:seed(H,M,S),
    %% 2) Generate first vowel
    VowelList = [random:uniform(26)+96],
    create_random_name(VowelList).
    
create_random_name(NameList) ->
    Name = list_to_atom(NameList),
    case whereis(Name) of
	undefined ->
	    %% Agent Name available
	    Name;
	_ ->
	    NewNameList = [random:uniform(26)+96|NameList],
	    create_random_name(NewNameList)
    end.

%%%%% TIME HANDLING FUNCTIONS

%% Creates a timestamp in milliseconds
%% NOTE: it will likely be negative, so do not use it for naming
timestamp()->
    NativeTime =
	erlang:monotonic_time(),
    erlang:convert_time_unit(NativeTime, native, millisecond).


%% Removes old entries from a history of timestamps
clean_history(History, infinity, _TimeStamp)->
      History;
clean_history(History, MaxTime, TimeStamp)->
    lists:filter(
      fun(Time) ->
	      abs(Time-TimeStamp) =< MaxTime
      end,
      History).





    
