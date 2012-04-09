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
%% @doc Provides all the functions necessary to transform eJason intermediate
%%      code to Erlang.


-module(jasonParser).

-compile(export_all).


generateInitialBeliefs(Beliefs)->
 %   io:format("CALLED WITH PARAMS ~n~p~n",[Beliefs]),
    Header = "%% Function to includeInitial beliefs.\n\n"++
	"addInitialBeliefs(Agent = #agentRationale{})->\n",
    {BeliefStrings,RuleStrings,FunNames} = sourceForInitialBeliefs(
					     Beliefs,{"","",[]}),
    %_SFuns = sourceForTermList(FunNames,","),
    io_lib:format(
      "-define(FunNames,~p).~n~n~s~n"++
      "\treasoningCycle:applyChanges(Agent,[\n~s\n\t\t{none}]).\n~n~s",
      [FunNames,Header, BeliefStrings,RuleStrings]).
        

sourceForInitialBeliefs([],Acc)->
    Acc;
sourceForInitialBeliefs([{atom,_line,Atom}|Beliefs],{AccB,AccR,
							      FunNames}) -> 

    SBelief = io_lib:format("\t\t{add_belief,{~p}},\n",
		      [Atom]),
    NewAccB = 
	io_lib:format("~s~s",
		      [AccB,SBelief]),
 
    
    sourceForInitialBeliefs(Beliefs,{NewAccB,AccR,FunNames});
sourceForInitialBeliefs([{formula,Atom,Terms,Label}|Beliefs],{AccB,AccR,
							      FunNames}) -> 
    SourceOfTerms = 
	case Terms of
	    [] ->
		"";
	    _ ->
%		io:format("AQUI: ~p",[Terms]),
		io_lib:format("~s",[sourceForTermList(Terms,",")])
	end,
    SourceOfLabel =
	case Label of 
	    []->
		"";
	    _ ->
%		io:format("Alli"),
		io_lib:format("~s",[sourceForTermList(Label,",")])
	end,

    SBelief = io_lib:format("\t\t{add_belief,{~p,{~s},[~s]}},\n",
		      [Atom,SourceOfTerms, SourceOfLabel])
,
    NewAccB = 
	io_lib:format("~s~s",
		      [AccB,SBelief]),
 
    sourceForInitialBeliefs(Beliefs,{NewAccB,AccR,FunNames});
sourceForInitialBeliefs([{{formula,Atom,Terms,_Label},
			 eJasonRule,Conditions}|Beliefs],{AccB,AccR,FunNames}) ->
    ParamVariableNamesType = getParamNamesWithType(Terms,[]),
    %io:format("ParamVariablesNamesType: ~p~n",[ParamVariableNamesType]),
   % io:format("Conditions: ~p~n",[Conditions]),


    VariablesInConditions_Dup = getParamNamesWithType(
		 Conditions, []),
   % io:format("VariablesInConditions_Dup: ~p~n",[VariablesInConditions_Dup]),


    AllVars_Type_Dup = 
	lists:append(ParamVariableNamesType, VariablesInConditions_Dup),
    


    %io:format("AllVarsWithDuplicates: ~p~n",[AllVars_Type_Dup]),

       
    AllVarsType = utils:removeDuplicates(AllVars_Type_Dup),


    AllVarsNoType = removeParamTypes(AllVarsType),
    %io:format("AllVarsType: ~p~n",[AllVarsType]),
    %io:format("AllVarsNoType: ~p~n",[AllVarsNoType]),

    


    NonVarParams = getNonVarParamNames(AllVarsType),

    ParamsNoType = removeParamTypes(ParamVariableNamesType),
 


    NumVars = length(AllVarsNoType),

    %io:format("AllVarsNoType in ~p: ~p~n",[Atom,AllVarsNoType]),

    VarIndexes = getIndexesForVars(ParamsNoType,AllVarsNoType,[]),
   
   
    NonVarIndexes = getIndexesForVars(NonVarParams,AllVarsNoType),


    {NewTerms,_NewVars} = utils:replaceNonVarsForVars(Terms,AllVarsNoType),


    SParams = sourceForTermList(NewTerms,","),
        
    
    SParams = sourceForTermList(NewTerms,","),

    SAllParams = sourceForTermList(NewTerms++NonVarParams,","),


    NumVarParams = lists:seq(1,length(VarIndexes)), 
    ReplacementsVars = lists:zip(VarIndexes,NumVarParams),
  
    NumNonVarParams = lists:seq(length(VarIndexes)+1,
				length(NonVarIndexes)+length(VarIndexes)), 
    

    ReplacementsNonVars = lists:zip(NonVarIndexes,NumNonVarParams),
    
    Replacements = lists:append(ReplacementsVars,ReplacementsNonVars),
    %io:format("ReplacementsVars: ~p~n",[ReplacementsVars]),
    %io:format("ReplacementsNonVars: ~p~n",[ReplacementsNonVars]),

 

    Params = sourceForTermList(Terms,","),
    SLoop = io_lib:format("~p(BBID)->~n\tRule= fun(~s)-> ~p(BBID,~s) "++
			  "end,~n\tRule.~n~n",
			  [Atom,Params,Atom,Params] ),
    SHead = io_lib:format("~p(BBID,~s) -> ~n\tInitVal = list_to_tuple(lists:duplicate(~p,'_')),~n"++
			  "\tVal0= utils:updateValuation([InitVal],~n"++
			  "\t[{~s}],~n\t~p),~n",
			  [Atom,Params]++
			  [NumVars]++
			  [SAllParams,Replacements]),
    SRuleBody =
	sourceForBody(Conditions,AllVarsNoType,length(ParamsNoType)),
    SRule = io_lib:format("~s~s~s",[SLoop,SHead,SRuleBody]),
    sourceForInitialBeliefs(Beliefs,{AccB,io_lib:format("~s~s",[AccR,SRule]),
				    [{Atom,length(Terms)}|FunNames]}).

	

sourceForBody(Conditions,RuleVariableNames,NumParams)->
    {LastValIndex,Source} =sourceForBody(Conditions,RuleVariableNames,NumParams,
					 0,1,""),
    io_lib:format("~s\tutils:makeParams(Val~p,[~s]).~n~n",  
	       [Source,LastValIndex,
		sourceForTermList(lists:seq(1,NumParams),",")]).



sourceForBody([],_,_ParamsNum,SourceValIndex,_NewValIndex,Acc)->
   %% ParamIndexes = lists:seq(1,ParamsNum),
   %% {SourceValIndex,io_lib:format("~s\tmakeParams(Val~p,[~s]).~n",
   %% 		  [Acc,SourceValIndex,sourceForTermList(ParamIndexes,",")])};
    {SourceValIndex,Acc};
sourceForBody([{formula,Atom,Terms,Label}|Conditions],Vars,ParamsNum,
    SourceValIndex,NewValIndex,Acc) ->
    
    TermParams = getParamList(Terms,[]),
    LabelParams = getParamList(Label,[]),
    FormulaVars = lists:append(TermParams,LabelParams),
    NumIndexes = getIndexesForVars(FormulaVars,
				   Vars,[]),
   % io:format("Code to call: ~p~nTermVars: ~p~nFormulaVars: ~p~nVars: ~p~nNumIndexes: ~p~n",
%	      [Atom,TermParams,FormulaVars,Vars,NumIndexes]),
    MakeParamsSource = io_lib:format("\tPar~p =utils:makeParams(Val~p,[~s]),~n",
				     [NewValIndex,SourceValIndex,
				      sourceForTermList(NumIndexes,",")]),
    %% SpawnSource = io_lib:format("\tP~p = spawn(?Name,~p,[]),~n",
    %% 				[NewValIndex,Atom]),
    %% ResSource = io_lib:format("\tRes~p = utils:getAll(P~p,Par~p,[]),~n",
    %% 			      [NewValIndex,NewValIndex,NewValIndex]),
    %% StopSource = io_lib:format("\tP~p ! stop,~n~n",[NewValIndex]),

    QueryBBSource = io_lib:format("\tRes~p = utils:query_bb(?Name,BBID,"++
				 "{~p,'_','_'},~n\tPar~p,?FunNames),~n",
				 [NewValIndex,Atom,NewValIndex]),
    NextValIndex = NewValIndex +1,


    ReplacementSource = replacementFromIndexes(NumIndexes,"",1),
    NewValSource = io_lib:format(
		     "\tVal~p = utils:updateValuation(Val~p,Res~p,[~s]),~n~n",
		     [NewValIndex,SourceValIndex,NewValIndex,
		      ReplacementSource]),
    NewAcc = io_lib:format("~s~s~s~s",
			   [Acc,MakeParamsSource,%SpawnSource,ResSource,
						%StopSource, 
			    QueryBBSource,NewValSource]),
    sourceForBody(Conditions,Vars,ParamsNum,NewValIndex,NextValIndex,NewAcc);
sourceForBody([{log_and,Cond1,Cond2}|Conditions],Vars,ParamsNum,
	      SourceValIndex,NewValIndex,Acc) ->
    {LastIndex1, SourceFor1} = sourceForBody([Cond1],Vars,ParamsNum,
					     SourceValIndex,NewValIndex,""),
    {LastIndex2, SourceFor2} = sourceForBody([Cond2],Vars,ParamsNum,LastIndex1,
					     LastIndex1+1,""),
    NewAcc = io_lib:format(
	       "~s~s~s~n",
	      [Acc,SourceFor1,SourceFor2]),
    sourceForBody(Conditions,Vars,ParamsNum,LastIndex2,LastIndex2+1,NewAcc);
sourceForBody([{log_or,Cond1,Cond2}|Conditions],Vars,ParamsNum,
	      SourceValIndex,NewValIndex,Acc) ->
    {LastIndex1, SourceFor1} = sourceForBody([Cond1],Vars,ParamsNum,
					     SourceValIndex,NewValIndex,""),
    {LastIndex2, SourceFor2} = sourceForBody([Cond2],Vars,ParamsNum,
					     SourceValIndex,LastIndex1+1,""),
    NewAcc = io_lib:format(
	       "~s~s~s\tVal~p = utils:valuation_or(Val~p,Val~p),~n~n",
	      [Acc,SourceFor1,SourceFor2,LastIndex2+1,LastIndex1,LastIndex2]),
    sourceForBody(Conditions,Vars,ParamsNum,LastIndex2+1,LastIndex2+2,NewAcc);
sourceForBody([{rel_assig,Left,Right}|Conditions],Vars,ParamsNum,
	      SourceValIndex,NewValIndex,Acc) ->

 %io:format("Left: ~p~nRight: ~p~n",
%    [Left,Right]),
 %   io:format("Vars ~p~n", [Vars]),


    LeftTermParam = getParamList([Left],[]),
    
    RightParams = getParamList([Right],[]),

    
    RightParamsType = getParamNamesWithType([Right],[]),

  
    %% io:format("FormulaVarsWithType: ~p~n",[FormulaVarsWithType]),
    %SLeft = sourceForTerm(Left),
    SRight = sourceForTerm(Right),
    
   % FormulaVars = lists:append(LeftTermParam,RightParams),
    
    RightNumIndexes = getIndexesForVars(RightParams,
				   Vars,[]),
    
    LeftNumIndexes = getIndexesForVars(LeftTermParam,
				   Vars,[]),
  

    MakeParamsSource = io_lib:format("\tPar~p =utils:makeParams(Val~p,[~s]),~n",
				     [NewValIndex,SourceValIndex,
				      sourceForTermList(RightNumIndexes,",")]),
    %io:format("MakeParams: ~s~n",[MakeParamsSource]),
    %io:format("FormulaVars: ~p~n",[FormulaVars]),
    
    SFun = io_lib:format(
	     "\tFun~p = fun ({~s}) -> {~s} end,~n",
	     [NewValIndex,sourceForTermList(RightParamsType,", "),
	      SRight]),
    
  %  io:format("SFun: ~s~n ",[SFun]),
    
    SRes = io_lib:format(
	     "\tRes~p = lists:map(Fun~p,Par~p),~n",
	     [NewValIndex,NewValIndex,NewValIndex]),
    
    NextValIndex = NewValIndex +1,
    
    ReplacementSource = replacementFromIndexes(LeftNumIndexes,"",1),
    
    NewValSource = io_lib:format(
		     "\tVal~p = utils:updateValuation(Val~p,Res~p,[~s]),~n~n",
		     [NewValIndex,SourceValIndex,NewValIndex,
		      ReplacementSource]),
    NewAcc = io_lib:format("~s~s~s~s~s",
			   [Acc,MakeParamsSource,SFun,SRes,NewValSource]),
    sourceForBody(Conditions,Vars,ParamsNum,NewValIndex,NextValIndex,NewAcc);   

sourceForBody([{Rel_Op,Left,Right}|Conditions],Vars,ParamsNum,
	      SourceValIndex,NewValIndex,Acc) ->

   %io:format("Left: ~p~nRight: ~p~n",
%    [Left,Right]),


    LeftTermParam = getParamList([Left],[]),
    
    RightParams = getParamList([Right],[]),

    LeftTermParamType = getParamNamesWithType([Left],[]),
    
    RightParamsType = getParamNamesWithType([Right],[]),

    FormulaVarsWithType = lists:append(LeftTermParamType,RightParamsType),
    
    %% io:format("FormulaVarsWithType: ~p~n",[FormulaVarsWithType]),
    SLeft = sourceForTerm(Left),
    SRight = sourceForTerm(Right),
    
%    io:format("Vars ~p~n", [Vars]),
%   io:format("LeftTermParams: ~p~nRightParams: ~p~n",
%    [LeftTermParam,RightParams]),

	%  io:format("SLeft: ~p~nSRight: ~p~n",
%[SLeft,SRight]),
    SRelOp = case Rel_Op of
		 rel_diff ->
		     "=/=";
		 rel_eq ->
		      "=:=";
		 rel_ge ->
		     ">=";
		 rel_gt ->
		       ">";
		 rel_lt ->
		      "<";
		 rel_le ->
		     "=<"
	     end,
    FormulaVars = lists:append(LeftTermParam,RightParams),
    
    NumIndexes = getIndexesForVars(FormulaVars,
				   Vars,[]),
    
    MakeParamsSource = io_lib:format("\tPar~p =utils:makeParams(Val~p,[~s]),~n",
				     [NewValIndex,SourceValIndex,
				      sourceForTermList(NumIndexes,",")]),
    %io:format("MakeParams: ~s~n",[MakeParamsSource]),
    %io:format("FormulaVars: ~p~n",[FormulaVars]),
    

%% This will end up giving a value. Make sure the results of the fun are tuples.
%% Or chang function updateValuation to accept bindings as number, not in a 
%% tuple
    SFun = io_lib:format(
	     "\tFun~p = fun ({~s}) -> ~s ~s ~s end,~n",
	     [NewValIndex,sourceForTermList(FormulaVarsWithType,", "),
	      SLeft,SRelOp,SRight]),
    
    %io:format("SFun: ~s~n ",[SFun]),
    
    SRes = io_lib:format(
	     "\tRes~p = lists:filter(Fun~p,Par~p),~n",
	     [NewValIndex,NewValIndex,NewValIndex]),
    
    NextValIndex = NewValIndex +1,
    
    ReplacementSource = replacementFromIndexes(NumIndexes,"",1),
    
    NewValSource = io_lib:format(
		     "\tVal~p = utils:updateValuation(Val~p,Res~p,[~s]),~n~n",
		     [NewValIndex,SourceValIndex,NewValIndex,
		      ReplacementSource]),
    NewAcc = io_lib:format("~s~s~s~s~s",
			   [Acc,MakeParamsSource,SFun,SRes,NewValSource]),
    sourceForBody(Conditions,Vars,ParamsNum,NewValIndex,NextValIndex,NewAcc).



getIndexesForVars(Var,AllVars)->
    getIndexesForVars(Var,AllVars,[]).
	
getIndexesForVars([],_,Acc)->
    lists:reverse(Acc);
getIndexesForVars([Var|Vars],AllVars,Acc) ->
    NewAcc = [indexOf(Var,AllVars)|Acc],
    getIndexesForVars(Vars,AllVars,NewAcc).

indexOf(Value,List)->
    indexOf(Value,List,1).

indexOf('_',_,_)->
    0;
indexOf(_,[],_)->
    -1;
indexOf(Value,[Value|_List],Count) ->
    Count;
indexOf(Value,[_OtherValue|List],Count)->
    indexOf(Value,List,Count+1).

replacementFromIndexes([],Acc,_)->
    Acc;
replacementFromIndexes([Index|Indexes],Acc,Count) ->
    NewAcc = case Count of 
		 1 -> 
		     io_lib:format("{~p,~p}",[Index,Count]) ;
		 _ ->
		     io_lib:format("~s,{~p,~p}",[Acc,Index,Count])
	     end,
    replacementFromIndexes(Indexes,NewAcc,Count+1).
		     
    
          
%% getParamNames(Terms)->
%%     getParamNames(Terms,[]).


removeParamTypes(Params)->
    removeParamTypes(Params,[]).

removeParamTypes([],Acc)->
    lists:reverse(Acc);
removeParamTypes([Param|Params],Acc) ->
    Res = case Param of
	      {_Type,Name} ->
		  Name;
	      _ ->
		  Param
	  end,
    removeParamTypes(Params,[Res|Acc]).

getParamNames(Terms,Acc)->
   Res= getParamNamesWithType(Terms,Acc),
   removeParamTypes(Res).




getParamList(Terms,Acc)->
   % io:format("LIST:~p~n",[Terms]),
    Res = getParamNamesWithType(Terms,Acc,all),
    removeParamTypes(Res).

getParamNamesWithType(Terms,Acc)->
    getParamNamesWithType(Terms,Acc,unique).
		


getParamNamesWithType([],Acc,_)->
    %io:format("Finishing getParamNamesWithType with Acc: ~p~n",[Acc]),
    lists:reverse(Acc);


getParamNamesWithType([{var,_Line,Var}|Terms],Vars,unique)->
    case lists:member(Var,Vars) of
	true->
	    getParamNamesWithType(Terms,Vars,unique);
	false ->
	    getParamNamesWithType(Terms,[{var,Var}|Vars],unique)
    end;
getParamNamesWithType([{var,_Line,Var}|Terms],Vars,all)->
    %io:format("VAR: ~p~n",[Var]),
    getParamNamesWithType(Terms,[{var,Var}|Vars],all);
getParamNamesWithType([{atom,_Line,Atom}|Terms],Vars,Mode)->
    getParamNamesWithType(Terms,[{atom,Atom}|Vars],Mode);
getParamNamesWithType([{formula,_Atom,Terms,Label}|Conditions],Vars,Mode) ->
    VarsTerms = getParamNamesWithType(Terms,lists:reverse(Vars),Mode),
    VarsTotal = getParamNamesWithType(Label,lists:reverse(VarsTerms),Mode),
    getParamNamesWithType(Conditions,lists:reverse(VarsTotal),Mode);
getParamNamesWithType([{rel_lt,Left,Right}|Conditions],Vars,Mode) ->
    VarsLeft = getParamNamesWithType([Left],lists:reverse(Vars),Mode),
    VarsTotal = getParamNamesWithType([Right],lists:reverse(VarsLeft),Mode),
    getParamNamesWithType(Conditions,lists:reverse(VarsTotal),Mode);
getParamNamesWithType([{rel_le,Left,Right}|Conditions],Vars,Mode) ->
    VarsLeft = getParamNamesWithType([Left],lists:reverse(Vars),Mode),
    VarsTotal = getParamNamesWithType([Right],lists:reverse(VarsLeft),Mode),
    getParamNamesWithType(Conditions,lists:reverse(VarsTotal),Mode);
getParamNamesWithType([{rel_gt,Left,Right}|Conditions],Vars,Mode) ->
    VarsLeft = getParamNamesWithType([Left],lists:reverse(Vars),Mode),
    VarsTotal = getParamNamesWithType([Right],lists:reverse(VarsLeft),Mode),
    getParamNamesWithType(Conditions,lists:reverse(VarsTotal),Mode);
getParamNamesWithType([{rel_ge,Left,Right}|Conditions],Vars,Mode) ->
    VarsLeft = getParamNamesWithType([Left],lists:reverse(Vars),Mode),
    VarsTotal = getParamNamesWithType([Right],lists:reverse(VarsLeft),Mode),
    getParamNamesWithType(Conditions,lists:reverse(VarsTotal),Mode);
getParamNamesWithType([{rel_eq,Left,Right}|Conditions],Vars,Mode) ->
    VarsLeft = getParamNamesWithType([Left],lists:reverse(Vars),Mode),
    VarsTotal = getParamNamesWithType([Right],lists:reverse(VarsLeft),Mode),
    getParamNamesWithType(Conditions,lists:reverse(VarsTotal),Mode);
getParamNamesWithType([{rel_diff,Left,Right}|Conditions],Vars,Mode) ->
    VarsLeft = getParamNamesWithType([Left],lists:reverse(Vars),Mode),
    VarsTotal = getParamNamesWithType([Right],lists:reverse(VarsLeft),Mode),
    getParamNamesWithType(Conditions,lists:reverse(VarsTotal),Mode);
getParamNamesWithType([{rel_assig,Left,Right}|Conditions],Vars,Mode) ->
    VarsLeft = getParamNamesWithType([Left],lists:reverse(Vars),Mode),
    VarsTotal = getParamNamesWithType([Right],lists:reverse(VarsLeft),Mode),
    getParamNamesWithType(Conditions,lists:reverse(VarsTotal),Mode);
getParamNamesWithType([{rel_decomp,Left,Right}|Conditions],Vars,Mode) ->
    VarsLeft = getParamNamesWithType([Left],lists:reverse(Vars),Mode),
    VarsTotal = getParamNamesWithType([Right],lists:reverse(VarsLeft),Mode),
    getParamNamesWithType(Conditions,lists:reverse(VarsTotal),Mode);
getParamNamesWithType([{arith_plus,Left,Right}|Conditions],Vars,Mode) ->
    VarsLeft = getParamNamesWithType([Left],lists:reverse(Vars),Mode),
    VarsTotal = getParamNamesWithType([Right],lists:reverse(VarsLeft),Mode),
    getParamNamesWithType(Conditions,lists:reverse(VarsTotal),Mode);
getParamNamesWithType([{arith_minus,Left,Right}|Conditions],Vars,Mode) ->
    VarsLeft = getParamNamesWithType([Left],lists:reverse(Vars),Mode),
    VarsTotal = getParamNamesWithType([Right],lists:reverse(VarsLeft),Mode),
    getParamNamesWithType(Conditions,lists:reverse(VarsTotal),Mode);
getParamNamesWithType([{arith_mult,Left,Right}|Conditions],Vars,Mode) ->
    VarsLeft = getParamNamesWithType([Left],lists:reverse(Vars),Mode),
    VarsTotal = getParamNamesWithType([Right],lists:reverse(VarsLeft),Mode),
    getParamNamesWithType(Conditions,lists:reverse(VarsTotal),Mode);
getParamNamesWithType([{arith_slash,Left,Right}|Conditions],Vars,Mode) ->
    VarsLeft = getParamNamesWithType([Left],lists:reverse(Vars),Mode),
    VarsTotal = getParamNamesWithType([Right],lists:reverse(VarsLeft),Mode),
    getParamNamesWithType(Conditions,lists:reverse(VarsTotal),Mode);
getParamNamesWithType([{arith_div,Left,Right}|Conditions],Vars,Mode) ->
    VarsLeft = getParamNamesWithType([Left],lists:reverse(Vars),Mode),
    VarsTotal = getParamNamesWithType([Right],lists:reverse(VarsLeft),Mode),
    getParamNamesWithType(Conditions,lists:reverse(VarsTotal),Mode);
getParamNamesWithType([{arith_mod,Left,Right}|Conditions],Vars,Mode) ->
    VarsLeft = getParamNamesWithType([Left],lists:reverse(Vars),Mode),
    VarsTotal = getParamNamesWithType([Right],lists:reverse(VarsLeft),Mode),
    getParamNamesWithType(Conditions,lists:reverse(VarsTotal),Mode);
getParamNamesWithType([{log_and,Cond1,Cond2}|Conditions],Vars,Mode) ->
    VarsCond1 = getParamNamesWithType([Cond1],lists:reverse(Vars),Mode),
    VarsTotal = getParamNamesWithType([Cond2],lists:reverse(VarsCond1),Mode),
    getParamNamesWithType(Conditions,lists:reverse(VarsTotal),Mode);
getParamNamesWithType([{log_or,Cond1,Cond2}|Conditions],Vars,Mode) ->
    VarsCond1 = getParamNamesWithType([Cond1],lists:reverse(Vars),Mode),
    VarsTotal = getParamNamesWithType([Cond2],lists:reverse(VarsCond1),Mode),
    getParamNamesWithType(Conditions,lists:reverse(VarsTotal),Mode);
getParamNamesWithType([{add_achievement_goal,Formula}|Conditions],Vars,Mode)->
    getParamNamesWithType([Formula|Conditions],Vars,Mode);
getParamNamesWithType([{action,_ActionType,Term}|Conditions],Vars,Mode) ->
    getParamNamesWithType([Term|Conditions],Vars,Mode);
getParamNamesWithType([{action,internal_action,_Packages,F}|Conditions],
		      Vars,Mode) ->
    getParamNamesWithType([F|Conditions],Vars,Mode);
getParamNamesWithType([{add_belief,_Atom}|Conditions],Vars,Mode) ->
    getParamNamesWithType(Conditions,Vars,Mode);
getParamNamesWithType([true|Conditions],Vars,Mode) ->
    getParamNamesWithType(Conditions,Vars,Mode);
getParamNamesWithType([S|Conditions],Vars,Mode) when is_list(S) ->
    getParamNamesWithType(Conditions,Vars,Mode);
getParamNamesWithType([{string,_,Str}|Conditions],Vars,Mode)  ->
    getParamNamesWithType(Conditions,[{string,Str}|Vars],Mode);  
getParamNamesWithType([{number,_,Num}|Conditions],Vars,Mode)  ->
    getParamNamesWithType(Conditions,[{number,Num}|Vars],Mode). 
 
 

sourceForTermList([],_)->
    "";
sourceForTermList(Terms,Separator)->
    Source = lists:map(fun jasonParser:sourceForTerm/1, Terms),
    string:join(Source,Separator).


sourceForTermListNoVars([],_)->
    "";
sourceForTermListNoVars(Terms,Separator)->
    Source = lists:map(fun jasonParser:sourceForTermNoVars/1, Terms),
    string:join(Source,Separator).


%% Changes variable names for '_'
sourceForTermNoVars({formula,Atom,Terms, Label})->
    case {Terms,Label} of
	{[],[]}->
	    io_lib:format("~p",[Atom]);
	_->
	    SourceOfTerms = 
		case Terms of
		    [] ->
			"{}";
		    _ ->
			io_lib:format("{~s}",[sourceForTermListNoVars(Terms,",")])
		end,
	    SourceOfLabel =
		case Label of 
		    []->
			"[]";
		    _ ->
			io_lib:format("[~s]",[sourceForTermListNoVars(Label,",")])
		end,
	    io_lib:format("{~p,~s,~s}",[Atom,SourceOfTerms,SourceOfLabel])
			
    end;
sourceForTermNoVars({list,[],[]})->
    "[]";
sourceForTermNoVars({list,Elements,Tail}) ->
    SourceOfElements = sourceForTermListNoVars(Elements,","),
    case Tail of
	[]->
	    io_lib:format("[~s]",[SourceOfElements]);
	_ ->
	    SourceOfTail = sourceForTermNoVars(Tail),
	    io_lib:format("[~s|~s]",[SourceOfElements,SourceOfTail])
    end;
sourceForTermNoVars({atom,_Lin,Atom}) ->
    io_lib:format("~p",[Atom]);
sourceForTermNoVars({var,_Lin,_Var}) ->
    io_lib:format("~p",['_']);
sourceForTermNoVars({string,_Lin,S}) ->
    io_lib:format("~p",[S]);
sourceForTermNoVars({number,_Lin,N}) ->
    io_lib:format("~p",[N]);
sourceForTermNoVars(A) ->
    io_lib:format("~p",[A]).


sourceForTermListVars([],_)->
    "";
sourceForTermListVars(Terms,Separator)->
    Source = lists:map(fun jasonParser:sourceForTermVars/1, Terms),
    string:join(Source,Separator).



sourceForTermVars({formula,Atom,Terms, Label})->
    case {Terms,Label} of
	{[],[]}->

	    io_lib:format("~p",[Atom]);
	_->
	    SourceOfTerms = 
		case Terms of
		    [] ->
			"{}";
		    _ ->
			io_lib:format("{~s}",[sourceForTermListVars(Terms,",")])
		end,
	    SourceOfLabel =
		case Label of 
		    []->
			"[]";
		    _ ->
			io_lib:format("[~s]",[sourceForTermListVars(Label,",")])
		end,
	    io_lib:format("{~p,~s,~s}",[Atom,SourceOfTerms,SourceOfLabel])
			
   end;
sourceForTermVars({list,[],[]})->
    "[]";
sourceForTermVars({list,Elements,Tail}) ->
    SourceOfElements = sourceForTermListVars(Elements,","),
    case Tail of
	[]->
	    io_lib:format("[~s]",[SourceOfElements]);
	_ ->
	    SourceOfTail = sourceForTermVars(Tail),
	    io_lib:format("[~s|~s]",[SourceOfElements,SourceOfTail])
    end;
sourceForTermVars({atom,_Lin,Atom}) ->
    io_lib:format("~p",[Atom]);
sourceForTermVars({var,_Lin,Var}) ->
    io_lib:format("~p",[Var]);
sourceForTermVars({string,_Lin,S}) ->
    io_lib:format("~p",[S]);
sourceForTermVars({number,_Lin,N}) ->
    io_lib:format("~p",[N]);
sourceForTermVars(A) ->
    io_lib:format("~p",[A]).



sourceForTerm({formula,Atom,Terms, Label})->
    case {Terms,Label} of
	{[],[]}->
	    io_lib:format("~p",[Atom]);
	_->
	    SourceOfTerms = 
		case Terms of
		    [] ->
			"{}";
		    _ ->
			io_lib:format("{~s}",[sourceForTermList(Terms,",")])
		end,
	    SourceOfLabel =
		case Label of 
		    []->
			"[]";
		    _ ->
			io_lib:format("[~s]",[sourceForTermList(Label,",")])
		end,
	    io_lib:format("{~p,~s,~s}",[Atom,SourceOfTerms,SourceOfLabel])
			
    end;
sourceForTerm({list,[],[]})->
    "[]";
sourceForTerm({list,Elements,Tail}) ->
    SourceOfElements = sourceForTermList(Elements,","),
    case Tail of
	[]->
	    io_lib:format("[~s]",[SourceOfElements]);
	_ ->
	    SourceOfTail = sourceForTerm(Tail),
	    io_lib:format("[~s|~s]",[SourceOfElements,SourceOfTail])
    end;
sourceForTerm({atom,_Lin,Atom}) ->
    io_lib:format("~p",[Atom]);
sourceForTerm({var,_Lin,Var}) ->
    io_lib:format("~s",[Var]);
sourceForTerm({string,_Lin,S}) ->
    io_lib:format("~p",[S]);
sourceForTerm({number,_Lin,N}) ->
    io_lib:format("~p",[N]);
sourceForTerm({arith_plus,Left,Right}) ->
    io_lib:format("~s+~s",[sourceForTerm(Left),sourceForTerm(Right)]);
sourceForTerm({arith_minus,Left,Right}) ->
    io_lib:format("~s-~s",[sourceForTerm(Left),sourceForTerm(Right)]);
sourceForTerm({arith_mult,Left,Right}) ->
    io_lib:format("~s*~s",[sourceForTerm(Left),sourceForTerm(Right)]);
sourceForTerm({arith_slash,Left,Right}) ->
    io_lib:format("~s/~s",[sourceForTerm(Left),sourceForTerm(Right)]);
sourceForTerm({arith_div,Left,Right}) ->
    io_lib:format("~sdiv~s",[sourceForTerm(Left),sourceForTerm(Right)]);
sourceForTerm({arith_mod,Left,Right}) ->
    io_lib:format("~srem~s",[sourceForTerm(Left),sourceForTerm(Right)]);
sourceForTerm({atom,Atom}) ->
    io_lib:format("~p",[Atom]);
sourceForTerm({var,Var}) ->
    io_lib:format("~s",[Var]);
sourceForTerm({string,S}) ->
    io_lib:format("~p",[S]);
sourceForTerm({number,N}) ->
    io_lib:format("~p",[N]);
%% sourceForTerm(A) when is_number(A)->
%%     io_lib:format("~p",[A]);
sourceForTerm(A) ->
    io_lib:format("~p",[A]).



parseTest()->
    {ok,Tokens} = scanner:getTokens("test2.txt"),
    {ok,S} = jasonGrammar:parse(lists:flatten(Tokens)),
    io:format(S).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%GOALS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


generateInitialGoals(Goals)->
    Header = "%% Function to include initial goals.\n\n",
	
    SGoals = sourceForGoals(Goals),
    case Goals  of
	[] ->
	    io_lib:format(
	      "~saddInitialGoals(Agent = #agentRationale{})->~n\tok.~n",
	      [Header]);
	_ ->
	    io_lib:format("~saddInitialGoals(Agent = #agentRationale{})->~n\t"
			  ++"InitGoalList=[~s],~n\treasoningCycle:"
			  ++"applyChanges(Agent,InitGoalList).~n~n",
			  [Header,SGoals])
    end.


sourceForGoals(Goals)->
    sourceForGoals(Goals,"").

sourceForGoals([],Acc)->
    Acc;
sourceForGoals([{add_achievement_goal,Goal}],Acc) ->
    SGoal = sourceForTermNoVars(Goal),
    %io:format("~p~n",[Goal]),
    NewAcc = 
	case Goal of
	    {atom, _,_}->
		io_lib:format(
		  "~s\t{addEvent,#event{type=external"++
		  ",~n\t\tbody={add_achievement_goal,~s}}}", [Acc,SGoal]);
	    {formula,_,_,_} ->
		io_lib:format(
		  "~s\t{addEvent,#event{type=external"++
		  ",~n\t\tbody={add_achievement_goal,~s}}}", [Acc,SGoal])
	end,
   NewAcc;
sourceForGoals([{add_achievement_goal,Goal}|Goals],Acc) ->
    SGoal = sourceForTermNoVars(Goal),
    %io:format("~s~n",[SGoal]),
    NewAcc = 
	case Goal of
	    {atom,_,_}->
		io_lib:format(
		  "~s\t{addEvent,#event{type=external"++
		  ",~n\t\tbody={add_achievement_goal,{~s}}}}", [Acc,SGoal]);
	    {formula,_,_,_} ->
		io_lib:format(
		  "~s\t{addEvent,#event{type=external"++
		  ",~n\t\tbody={add_achievement_goal,~s}}}", [Acc,SGoal])
	end,
    NewAcc,
    sourceForGoals(Goals,NewAcc).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%PLANS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generatePlans(Plans)->
    generatePlans(Plans,"",1,[]).

generatePlans([],SAllPlans,_,PlanNameBindingsList)->
    %io:format("PlanNameList: ~p~n",[PlanNameList]),
    SPlanBase = generatePlanRecords(PlanNameBindingsList),
    io_lib:format("~s~s~n~n",[SPlanBase,SAllPlans]);
generatePlans([Plan|Plans],Acc,PlanNum,PlanNameBindingsList) ->
    %io:format("PLAN: ~p~n",[PlanNameList]),
    {PlanName,SPlan,Bindings} = sourceForPlan(Plan,PlanNum),
    NewAcc = io_lib:format("~s~s",[Acc,SPlan]),
    generatePlans(Plans,NewAcc,PlanNum+1,[{PlanName,Bindings}|PlanNameBindingsList]).



getNonVarParamNames(Params)->
    getNonVarParamNames(Params,[]).

getNonVarParamNames([],Acc)->
    lists:reverse(Acc);
getNonVarParamNames([Param|Params],Acc) ->
    case Param of
	{var,_}->
	    getNonVarParamNames(Params,Acc);
	{_,Name} ->
	    getNonVarParamNames(Params,[Name|Acc]);
	_ ->
	    getNonVarParamNames(Params,Acc)
    end.



sourceForTestHandlingPlan() ->
    STrigger ="ejason_standard_test_goal_handler_trigger(\n"++
	"\t{add_test_goal,Query})->\n"++
	"\t{true, [Query]};\n"++
	"ejason_standard_test_goal_handler_trigger(_)->\n"++
	"\tfalse.\n",
    
    SContext = "ejason_standard_test_goal_handler_context(_BBID,Query)->\n"++
	"\t[Query].\n",
    
    SBody = "ejason_standard_test_goal_handler_body(BBID,[Query])->\n"++ 
	"\tRes = utils:resolve_test_goal(Query,?Name,BBID,?FunNames),\n"++
	"\t[{finished,Res}].\n",
    io_lib:format("~s~n~s~n~s~n",[STrigger,SContext,SBody]).
    





sourceForPlan([],_)->
    {"","", []};
sourceForPlan({{trigger,Trigger},{context,Context},{body,Body}},PlanNum)->
    %io:format("Trigger: ~p~n",[Trigger]),
    PTrigger = getParamNamesWithType([Trigger],[]),
    %io:format("Vars Trigger: ~p~n",[PTrigger]),
    
    PTriggerContext = getParamNamesWithType([Context],lists:reverse(PTrigger)),
    %io:format("VarsContext: ~p~n",[PTriggerContext]),

    VarsType = getParamNamesWithType(Body,lists:reverse(PTriggerContext)),

    %ParamsForTrigger = removeParamTypes(PTrigger),

    NonVarParams = getNonVarParamNames(VarsType),
    %io:format("Vars Before: ~p~n",[VarsType]),
    Vars = utils:removeDuplicates(removeParamTypes(VarsType)),
    %io:format("Vars After: ~p~n",[Vars]),

    {PlanName,STrigger} = sourceForPlanTrigger(Trigger,PlanNum,PTrigger,Vars,NonVarParams),
    SContext = sourceForPlanContext(Context,PlanName,length(PTrigger),Vars),
    {SBody,Bindings} = sourceForPlanBody(Body,PlanName,Vars,PTrigger),
    {PlanName,io_lib:format("~s~s~s~n",[STrigger,SContext,SBody]),Bindings}.


sourceForPlanTrigger({PlanType,{formula,Atom,Terms,Label}},
		     PlanNum,ParamsWithType,Vars,NonVarParams)->
%% TODO: many unreadable checks. Probably should be done in the calling
%% function or in an auxiliary function

    %io:format("Params:~p~nVars:~p~n",[ParamsWithType,Vars]),
    ParamsNoType = removeParamTypes(ParamsWithType),
 
    NumVars = length(Vars),
    VarIndexes = getIndexesForVars(ParamsNoType,Vars,[]),
   
     % SIndexes = sourceForTermList(Indexes,","),
    
    NonVarIndexes = getIndexesForVars(NonVarParams,Vars),
        
    Name = io_lib:format("~p_~p",[Atom,PlanNum]),
  
    {NewTerms,_NewVars} = utils:replaceNonVarsForVars(Terms,Vars),
    %io:format("NewTerms: ~p~nNewVars: ~p~n",[NewTerms,NewVars]),


    SParams = sourceForTermList(NewTerms,","),

    SLabel = sourceForTermListNoVars(Label,","),

   %io:format("VarIndexes: ~p~nNonVarIndexes: ~p~n",[VarIndexes,NonVarIndexes]),

   %io:format("Terms: ~p~nNonVarParams:~p~n",[Terms,NonVarParams]),

%Included in the first initial valuation
    SAllParams = sourceForTermList(NewTerms++NonVarParams,","),
    %io:format("SParams: ~p~n",[SParams]),


    NumVarParams = lists:seq(1,length(VarIndexes)), 
    ReplacementsVars = lists:zip(VarIndexes,NumVarParams),
  
    NumNonVarParams = lists:seq(length(VarIndexes)+1,
				length(NonVarIndexes)+length(VarIndexes)), 
    ReplacementsNonVars = lists:zip(NonVarIndexes,NumNonVarParams),
    
    Replacements = lists:append(ReplacementsVars,ReplacementsNonVars),

    %io:format("Replacements: ~p~n",[Replacements]),

    {Name,io_lib:format(
	    "~s_trigger({~p,{~p,{~s},[~s]}})->~n\t"++

	    "InitValuation = list_to_tuple(lists:duplicate(~p,'_')),~n"++
	    "\tResVal = utils:updateValuation([InitValuation],~n"++
	    "\t[{~s}],~n\t~p),~n"++
	    "\tcase ResVal of~n\t\t[]->~n\t\t\tfalse;~n"++
	    "\t\t_->~n\t\t\t{true,ResVal}\n\tend;~n"
	    ++"~s_trigger(_A)->~n\tfalse.~n",
	    [Name,PlanType,Atom,SParams,SLabel]++
	    [NumVars]++
	    [SAllParams,Replacements]++
	    [Name])};
sourceForPlanTrigger({PlanType,{atom,_Line,Atom}},PlanNum,_Params,Vars,NonVarParams) -> 
    Name = io_lib:format("~p_~p",[Atom,PlanNum]),
  % io:format("Params:~p~nVars:~p~n",[Params,Vars]),
    NumVars = length(Vars),
   % Indexes = getIndexesForVars([Atom],Vars,[]),
   % SIndexes = sourceForTermList(Indexes,","),
    NonVarIndexes = getIndexesForVars(NonVarParams,Vars),
    NumNonVarParams = lists:seq(1,length(NonVarIndexes)), 
    Replacements = lists:zip(NonVarIndexes,NumNonVarParams),
    SParams = sourceForTermListNoVars(NonVarParams, ","),
    
    
    
    {Name,
     io_lib:format(
       "~s_trigger({~p,~p})->~n\t"++
       "InitValuation = list_to_tuple(lists:duplicate(~p,'_')),~n"++
       "\tResVal = utils:updateValuation([InitValuation],[{~s}],~p),~n"++
       "\tcase ResVal of~n\t\t[]->false;~n\t\t_->\t{true,ResVal}\n\tend;~n"
       "~s_trigger(_A)->~n\tfalse.~n",
       [Name,PlanType,Atom,NumVars, SParams, Replacements]++
       [Name])}.


sourceForPlanContext(true,PlanName,_NumParams,_Vars)->
    io_lib:format("~s_context(_BBID,InitVal)->~n\tInitVal.~n",[PlanName]);
sourceForPlanContext(Context,PlanName,NumParams,Vars) ->
    {LastVal,SContext} = 
	case Context of
	    {formula,_,_,_}->
		sourceForBody([Context],
			      Vars,NumParams,0,1,"");
	    {log_or,_,_} ->
		sourceForBody([Context],
			      Vars,NumParams,0,1,"");
	    {log_and,_,_} ->
		sourceForBody([Context],
			      Vars,NumParams,0,1,"");
	    _ ->
		io:format("INVALID CONTEXT:~n~p~n",[Context]),
		{-1,"ERROR"}
	end, 
    io_lib:format(
      "~s_context(BBID,InitVal)->~n\tVal0 = "++
      "case InitVal of ~n\t\tA when is_list(A) -> InitVal;\n"++
      "\t\tA when is_tuple(A) -> [A]\n end,~n"++
      "~s\tVal~p.~n",
      [PlanName,SContext,LastVal]).
    


sourceForPlanBody([],_PlanName,_Vars,_PTrigger)->
    ok;%%ERRO!
sourceForPlanBody(Actions,PlanName,Vars,PTrigger) ->

    %io:format("Params:~p~nVars:~p~n",[ParamsWithType,Vars]),
    ParamsNoType = removeParamTypes(PTrigger),
    ParamIndexes = getIndexesForVars(ParamsNoType,Vars,[]),
   
     % SIndexes = sourceForTermList(Indexes,","),



%% CORRECT, BUT NOT ELEGANT; MAYBE NEEDS TO BE CHANGED
    NumActions = length(Actions),
%    io:format("Plan Body of ~s has ~p formulas.~n",[PlanName,NumActions]),

    VarCopies = lists:duplicate(NumActions,Vars),
    ActionsNumerated = lists:zip3(Actions,lists:seq(1,NumActions),VarCopies),
    SourceActions = [{X,Y,Z,W}|| {X,Y,Z}<-ActionsNumerated, W <- [PlanName]],

%    io:format("SourceActions is ~p~n",[SourceActions]),


    SLastFormula= io_lib:format(
		    "~n~s_body_last_formula(Valuation)-> ~n"++
		    "\t[{finished,utils:makeParams([Valuation],~p)}].",
		    [PlanName, ParamIndexes]),
  
    ActionTuples = lists:map(fun jasonParser:sourceForAction/1, 
			      SourceActions),

%    io:format("ActionTuples is ~p~n",[ActionTuples]),


    {SActions,Bindings} = actionTupleSplit(ActionTuples),

    SBody = string:join([SLastFormula|SActions],"\n"),

    {SBody,Bindings}.
    

actionTupleSplit(List)->
    actionTupleSplit(List,[],[]).

actionTupleSplit([],Acc1,Acc2)->
    {lists:reverse(Acc1),lists:reverse(Acc2)};
actionTupleSplit([{ActionSource,Binding}|Rest],Acc1,Acc2) ->
    actionTupleSplit(Rest,[ActionSource|Acc1],[Binding|Acc2]).


%%%%%%%%%%%%SOURCE FOR ACTION%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Generates the pairs {Source, Bindings} for each formula in a plan body
sourceForAction({{action,ActionType,Packages,F={formula,_Atom,_Terms,_Label}},
		 NumAct,Vars,FunName})-> 
    %% Is an internal action, along with package name
    sourceForAction({{action,{ActionType,Packages},
		      F={formula,_Atom,_Terms,_Label}},	
		     NumAct,Vars,FunName});
sourceForAction({{action,ActionType,{atom,_Line,Atom}},NumAct,Vars,FunName})->
    sourceForAction({{action,ActionType,
		      {formula,Atom,[],[]}},NumAct,Vars,FunName});
sourceForAction({{action,ActionType,F={formula,_Atom,_Terms,_Label}},
		 NumAct,Vars,FunName})->

    SFormula =  %case ActionType of
		 %   remove_add_belief ->
		%	sourceForTermNoVars(F);
		%    _ ->
			sourceForTermVars(F),
		%end,

%    io:format("SFormula: ~p~n",[SFormula]),
    ParamsF = getParamNames([F],[]),
%    io:format("ParamsF: ~p~n",[ParamsF]),
    
    Indexes = getIndexesForVars(ParamsF,Vars,[]),
 %   io:format("Indexes: ~p~n",[Indexes]),
    
    Bindings  = lists:zip(Indexes,lists:seq(1,length(Indexes))),
  %  io:format("Bindings: ~p~n",[Bindings]),


    SEvent = 
	case ActionType of
	    true -> 
		"";
	    {internal_action, Packages }->
		io_lib:format(
		  "\tok = utils:execute(?Name,?Internal,~s,Element),\n",
		  [Packages]);
	    {external_action, Packages }->
		io_lib:format(
		  "\tok = utils:execute(?Name,?Internal,~s,Element),\n",
		  [Packages]);
	    _->
		io_lib:format(
		"\tNewEvent = utils:~p(Element),\n",
		  [ActionType])
	end,
    SRes = 
	case ActionType of
	    {internal_action,_}->
		"\tRes = [{stutter_action}],\n";
	    {external_action,_}->
		"\tRes = [{stutter_action}],\n";
	    true->
		"\tRes = [{stutter_action}],\n";
	    _->
		"\tRes = [NewEvent],\n"
	end,

    SAction = io_lib:format(
		"~n~s_body_formula_~p(BBID,Valuation)-> \n"++
		"\tVarNames = ~p,\n"++%TODO: MEJORAR
		"\tElement = utils:unify_vars(~s,Valuation,VarNames),~n"
		"~s~s\tRes.~n",
		[FunName,NumAct]++
		[Vars]++
		[SFormula]++
		[SEvent,SRes]),


   % io:format("SAction: ~s~nBindings:~p~n",[SAction,Bindings]),
    {SAction, Bindings}.





		       
%% Converts an external action into an internal action after finding a dot
parseInternalAction({no_package},{action, external_action,F}) ->
    {action,internal_action,"'.'",F};
parseInternalAction({package,P},{action,external_action,F}) ->
    {action,internal_action,io_lib:format("~p.",[P]),F};
parseInternalAction({no_package},A={action,internal_action,_,_}) ->
    A;%%Duplicated dot
parseInternalAction({package,P},{action,internal_action,Packages,F}) ->
   {action,internal_action,io_lib:format("~p'.'~s",[P,Packages]),F}.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% ERLANG AGENTS
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generateAgentStart(Name)->
    Module = io_lib:format("-module(~p).~n~n",[Name]),
    Export = "-compile(export_all).\n\n",
    Import = "-include(\"macros.hrl\").\n\n",
    NameMacro = io_lib:format("-define(Name,~p).~n",[Name]),
    InternalMacro = io_lib:format("-define(Internal,~p).~n",[Name]),
    EnvironmentMacro =io_lib:format("-define(Environment,~p).~n",[Name]),
    Start = io_lib:format(
	      "start()->~n\tstart(0).~n~n~n"++
	      "start(Num)->~n"
	      "\tAgName= utils:register_agent(Num,self(),~p),~n"++
	      "\tAgent0 = reasoningCycle:"++
	      "start(AgName,[],[],beliefbase:start()),~n", [Name]),
    Init = "\tAgent1 = addInitialBeliefs(Agent0),\n"++
	"\tAgent2 = addInitialGoals(Agent1),\n"++
	"\tAgent3 = addPlans(Agent2),\n"++
	"\treasoningCycle:reasoningCycle(Agent3).\n\n",
    io_lib:format("~s~s~s~s~s~s~s~s",
		  [Module,Export,Import,
		   NameMacro,InternalMacro,
		   EnvironmentMacro,Start,Init]).   




parseAgents([])->
    ok;
parseAgents([Name|Names]) when is_atom(Name)->
    {ok,Tokens} = scanner:getTokens(io_lib:format("~p.asl",[Name])),
    {ok,AgentPart2} = jasonGrammar:parse(lists:flatten(Tokens)),
    AgentPart1 = generateAgentStart(Name),
    AgentPart3 = sourceForTestHandlingPlan(),
    SAgent = io_lib:format("~s~s~n~s",
			   [AgentPart1,AgentPart2,AgentPart3]),
    AgentFileName = io_lib:format("~p.erl",[Name]),
    {ok, AgentFile} = file:open(AgentFileName,[raw,write]),
    file:write(AgentFile,SAgent),
    file:close(AgentFile),
    parseAgents(Names).



generatePlanRecords(PlanNameBindingsList)->
    
    SPlans = lists:map(fun generatePlan/1,PlanNameBindingsList) ++
	[generate_test_goal_handler_plan()],

    Source = string:join(SPlans,",\n"),
    
			%io:format("PLANS: ~s~n",[Source]),
    io_lib:format("addPlans(Agent = #agentRationale{})->~n"++
		  "Agent#agentRationale{plans = [~n~s]}.~n~n",
		  [Source]).



generate_test_goal_handler_plan()->
    "\t#plan{trigger=fun ?Name:ejason_standard_test_goal_handler_trigger/1,\n
\tbody=[fun ?Name:ejason_standard_test_goal_handler_body/2],\n
\tcontext=fun ?Name:ejason_standard_test_goal_handler_context/2}".


generatePlan({PlanName,Bindings})->
    SBody = generatePlanRecordBody(PlanName,Bindings,1,""),
    
    io_lib:format(
      "\t#plan{trigger=fun ?Name:~s_trigger/1 ,~n"++
      "\tbody=~s,"++
      "~n\tcontext=fun ?Name:~s_context/2}",[PlanName,SBody,PlanName]).



generatePlanRecordBody(PlanName,[],_,Acc)->
    io_lib:format(
      "\t\t[~s,\nfun ~s_body_last_formula/1]", 
      [string:join(lists:reverse(Acc),",\n\t\t"), PlanName]);
generatePlanRecordBody(PlanName,[SingleBinding|Bindings],Num,Acc)->
    Formula= io_lib:format("{fun ~s_body_formula_~p/2,~p}",
			   [PlanName,Num,SingleBinding]),
    generatePlanRecordBody(PlanName,Bindings,Num+1,[Formula|Acc]).
    
		
