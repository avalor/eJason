-module(variables).

-compile(export_all).
-include("macros.hrl").
-include("parser.hrl").
-include("variables.hrl").





%siguiente(Bindings,Ejason_X,Ejason_Y) ->  
%    try 
	%% Add Params matches to BB
%	{Param1, NewBindings1} =
%          	variables:try_param_match(Bindings,id_var_x,Ejason_X),
%	{Param2, NewBindings2} =
%          	variables:try_param_match(BB,id_var_y,Ejason_Y),


%	Params = [Param1,Param2],
%    catch
%	exit:param_does_not_match ->%
%	    false
%    end,
%    Fun = fun (X) -> siguiente_1(Params,X) end,
%    ItList = iterator:create_iterator([NewBindings2]),
%    iterator:create_iterator_fun(ItList,Fun).
    


%siguiente_1(Params,Bindings)->
%    VarX   % io:format("No further valuation for ~p~n",[Bind]),
%    Bind.
% = variable:find(BB,id_var_x)...
%    
 %   case belief_base:query_bb({module,function,[VarX,VarY]}) of
%	false ->
%	    false;
%	It when is_fun(It) ->
%	    Fun = fun(NewBindings) ->
%			  NewVarX = variable:find(NewBindings,id_var_x)...
%			      NewVarY = variable:find(NewBindings,id_var_y),
%			  %%if last ->  [NewVarX,NewVaY] end
%			  NewBB = update(BB, X),
%			  ?Module:siguiente_2(Params,NewBB) end,
%    	    iterator:create_iterator_fun(It,Fun)
%    end.
    

%siguiente_2(Params,Bindings)->
%    return_params(Params,Bindings).




%%%%%%CASE 1   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Valuate param variables
       
%	X = variables:valuate(NewBBID2,Var_X),
%	Y = variables:valuate(NewBBID2,Var_Y), %% unbound

%% Execute function 	
%	NewY = X + 1,
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%CASE 2   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Valuate param variables
       
%	X = variables:valuate(NewBBID2,Var_X),
%	Y = variables:valuate(NewBBID2,Var_Y), %% unbound

%% Execute function 	
%	NewY = X + 1,
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%% Add new matches to BB	
%	NewVar_X = Var_X,
%	NewVar_Y = variables:bind(EjasonY,NewY),
	
%% Return new BB
%	variables:match(NewBBID2,VarY,NewVarY)
%    catch
%	exit:parser ->
%	    false
%    end.
	    
	
%    Fun1 = fun ({X, 1}) -> {X+1} end,
%    Res1 = lists:map(Fun1,Par1),
%    Val1 = utils:updateValuation(Val0,Res1,[{2,1}]),
%    
%    utils:makeParams(Val1,[1,2]).


%% Returns a variable without TS in the bind
get_valuated_var_from_timestamp(TS={I1,I2,I3},Bindings)
  when is_integer(I1), is_integer(I2), is_integer(I3)->
    Var = get_var_from_timestamp(TS,Bindings),
  %  io:format("Single var: ~p~n",[Var]),
    get_valuated_var_from_timestamp(Var,Bindings);
get_valuated_var_from_timestamp(Predic = {Pred,Args,Annot},
				Bindings) when is_tuple(Args), 
					       is_list(Annot)-> 
    
    get_valuated_var_from_timestamp(#var{bind = Predic},Bindings);

get_valuated_var_from_timestamp(#var{bind = {Pred,Args,Annot}},
				Bindings) when is_tuple(Args), 
					       is_list(Annot)->
    Fun = fun (X) -> get_valuated_var_from_timestamp(X,Bindings)end,
  %  io:format("Annot: ~p~n",[Annot]),
    {get_valuated_var_from_timestamp(Pred,Bindings),
     list_to_tuple(lists:map(Fun,tuple_to_list(Args))),
     lists:map(Fun,Annot)};
get_valuated_var_from_timestamp(Term,_Bindings ) ->
   % io:format("Term: ~p~n",[Term]),
    Term.



%% Checks whether an input parameter matches the pattern expected
%% The input parameters will be bound to the "known variables"
%% 
try_param_match(Bindings,TimeStamp,Param=#var{})->
  % io:format("AQUI~n~n~n"),
    ParamVar = get_var_from_timestamp(TimeStamp,Bindings),
    OldVar = get_valuated_var_from_timestamp(TimeStamp,Bindings),
    
%    io:format("Param: ~p~n Expected Param: ~p~n",[Param,OldVar]),

    Res=
	case match_vars(Param,OldVar) of
	    false->
%		io:format("Param: ~p~n does not match expected variable: ~p~n",
%			  [Param,OldVar]),
		exit(param_does_not_match);
	    Match when is_record(Match,var)-> 
%		io:format("Match: ~p~n",[Match]),
		NewParam = Param#var{
			     bind = OldVar#var.timestamp},
		%io:format("NewParam: ~p~n",[NewParam]),

		NewVar = Match#var{name = OldVar#var.name,
				   timestamp = TimeStamp},
		NewBB = update(Bindings,[NewVar]),
		{NewParam,NewBB};
	    NewVars when is_list(NewVars)->
%		io:format("ParamVar: ~p~n",[ParamVar]),
%		io:format("NewVarsMatched: ~p~n",[NewVars]),
		
		NewParam = 
		    case OldVar of
			_ when is_record(OldVar,var)->
			
			    Param#var{%unbound_vars = OldVar,
			      bind = OldVar#var.timestamp};
			{_,_,_}->
			    ParamVar
		    end,
%		io:format("NewParam: ~p~n",[NewParam]),

		NewBB = update(Bindings,NewVars),
		{NewParam,NewBB}
	end,
%    io:format("Try_param_matchRes: ~p~n",[Res]),
    Res;

try_param_match(Bindings,TimeStamp,Param) -> % Param is not a variable 
    %io:format("Param in try_param_match: ~p~n",[Param]),
    try_param_match(Bindings,TimeStamp,
		    #var{is_ground = true, bind = Param}). 

    

match_vars(Var= #var{bind = Bind},#var{bind = Bind}) ->
   Var;
match_vars(Var1= #var{bind = Bind1},Var2=#var{bind = Bind2}) ->
%   io:format("Bind1: ~p~nBind2: ~p~n",
%	      [Bind1,Bind2]), 
    case {Bind1,Bind2} of
	{?UNBOUNDVAR,_}->
	    Var2;
	{_,?UNBOUNDVAR} ->
	    Var1;
	{Pred1=  {_Name1,_Args1,_Annot1},
	 Pred2 = {_Name2,_Args2,_Annot2}} ->
	    case match_predicates(Pred1,Pred2) of
		It when is_function(It)->
		    iterator:first(It);
		false ->
		    false
	    end;
	{{Bind2,{},_},_}->
	    Var2;
	{{Name,{},_},_} when is_record(Name,var)->
	    match_vars(Name,Bind2);	
	_ when is_atom(Bind1),
	       is_atom(Bind2)->
	    false;
	_ ->
	    false
    end;
match_vars(Var1= #var{},TS={T1,T2,T3}) 
  when is_number(T1),is_number(T2),is_number(T3)->
    Var1#var{timestamp = TS};
match_vars(TS={T1,T2,T3},Var2= #var{}) 
  when is_number(T1),is_number(T2),is_number(T3)->
        Var2#var{timestamp = TS};
match_vars(Else,TS={T1,T2,T3}) 
  when is_number(T1),is_number(T2),is_number(T3)->
    #var{timestamp = TS,
	bind = Else};
match_vars(TS={T1,T2,T3},Else) 
  when is_number(T1),is_number(T2),is_number(T3)->
    #var{timestamp = TS,
	bind = Else};
%% Added later
match_vars(Var1 = #var{timestamp = TS}, Pred = {_PName,Args,Annot} ) 
  when is_tuple(Args), is_list(Annot) ->
    match_vars(Var1,#var{bind = Pred,timestamp = TS});
match_vars(Pred = {_PName,Args,Annot},Var2 = #var{timestamp = TS} ) 
  when is_tuple(Args), is_list(Annot) ->
    match_vars(#var{bind = Pred,timestamp = TS},Var2);
match_vars(Atom,Var2 = #var{timestamp = TS} ) 
  when is_atom(Atom) ->
    match_vars(#var{bind = Atom, timestamp = TS},Var2);
match_vars(Var1 = #var{timestamp = TS},Atom ) 
  when is_atom(Atom) ->
    match_vars(#var{bind = Atom, timestamp = TS },Var1);
match_vars(Number,Var2 = #var{timestamp = TS} ) 
  when is_number(Number) ->
    match_vars(#var{bind = Number,timestamp = TS },Var2);
match_vars(Var1 = #var{timestamp = TS},Number ) 
  when is_number(Number) ->
    match_vars(#var{bind = Number,timestamp = TS },Var1);
match_vars( Pred1 = {PName1,Args1,Annot1} ,Pred2 = {PName2,Args2,Annot2} ) 
  when is_tuple(Args1), is_list(Annot1),is_tuple(Args2), is_list(Annot2) ->
%       io:format("[Variables] Pred1: ~p~nPred2: ~p~n",[Pred1,Pred2]),

   Fun = fun ({X,Y}) -> match_vars(X,Y) end,
    ArgsList = lists:zip(tuple_to_list(Args1),
			 tuple_to_list(Args2)),

    Res = {match_vars(PName1,PName2),
     list_to_tuple(lists:map(Fun, ArgsList)),
     Annot1++Annot2},
 %   io:format("Result: ~p~n",[Res]),
    Res;

match_vars(P1,P2) ->
    io:format("[variables:match_vars/2, error] :P1: ~p~nP2: ~p~n",[P1,P2]),
    exit(error).   








%% Calculates the new Bindings for the annotations
%% Queries only contains timestamps to be matched later
%% Returns an iterator that generates tuples 
%%{AllPlanBindings,BindingsFromAnnotations}. There may be many  
%% matching possibilities. 
try_annot_matches(Bindings,Annotations,Queries)->
   % io:format("Annotations: ~p~nQueries: ~p~n",[Annotations,Queries]),
    NewAnnotations =
	fully_valuate(Annotations),
%    io:format("NewAnnotations: ~p~n",[NewAnnotations]),

    Iterator = try_annot_match({Bindings,[]},NewAnnotations,Queries),
 %   io:format("Returning: ~p~n",[ iterator:get_all(Iterator)]),
    Iterator.


%% Returns a tuple {AllPlanBindings,BindingsFromAnnotations}
try_annot_match({Bindings,AnnotBindings},Annotations,[])->
    iterator:create_iterator([{Bindings,AnnotBindings}]);
try_annot_match({Bindings,AnnotBindings},Annotations,[Query|Rest]) ->
    RealQuery = utils:valuate(Bindings,Query),
%    io:format("Real annot query: ~p~n",[RealQuery]),
    Fun = fun (NewBindings) ->
		%  io:format("NewBindings from bb:matchannotation: ~p~n",
	       %	    [NewBindings]),
			% Allows backtracking when matching annotations
		  FinalBindings = update(Bindings,NewBindings),
		  try_annot_match({FinalBindings,
				   AnnotBindings ++NewBindings},
				  Annotations,Rest) end,
    ItAnnotations = belief_base:match_annotations(Annotations,RealQuery),
%   io:format("bb matched annots: ~p~n",[iterator:get_all(ItAnnotations)]),

    iterator:create_iterator_fun(
      ItAnnotations,
      Fun).


%% Used to match the param predicates
%% Returns an iterator
match_predicates( {Name1,Args1,Annot1},
		 {Name2,Args2,Annot2})->
    case match_vars(Name1,Name2) of
	false->
	    false;
	Name ->
	    case match_arguments(tuple_to_list(Args1),
				 tuple_to_list(Args2)) of
		false ->
		    false;
		Args ->
		    F = fun (X) ->
				belief_base:match_annotations(Annot1,X)
			end,
		    ItListAnnots = 
			lists:map(F,Annot2),
		    CheckFun = 
			fun (X) ->
				FinalMatches = [Name|Args] ++X,
				case belief_base:check_consistency(
				       FinalMatches) of
				    false ->
					false;
				    List ->
					[utils:erase_repeated_vars(List)]
				end
			end,
		    ItRes = iterator:combine(ItListAnnots,CheckFun),
		    %io:format("MatchedAnnots: ~p~n",
		%	      [iterator:first(ItRes)]),
		    ItRes
		    
		    
		    %case belief_base:match_annotations(Annot1,
		%				       Annot2) of
		%	It when is_function(It) ->
		%	    io:format("MatchedAnnots: ~p~n",
		%		      [iterator:first(It)]),
%		%	    iterator:first(It);
		%	    [Name|Args] ++iterator:first(It);
		%	
		%	_ ->
		%	    false
		 %   end
	    
	    end
    end.			     


match_arguments(Args1,Args2) when length(Args1) =/= length(Args2) ->
    false;
match_arguments(Args1,Args2) ->
    match_arguments(Args1,Args2,[]).

match_arguments([],[],Acc) ->
    lists:reverse(Acc);
match_arguments([Arg1|Rest1],[Arg2|Rest2],Acc) ->
    case match_vars(Arg1,Arg2) of
	false ->
	    false;
	Match ->
	    match_arguments(Rest1,Rest2,[Match|Acc])
    end.





gather_unbound_vars(Vars)->
    gather_unbound_vars(Vars,[]).

gather_unbound_vars([],Acc)->
    Acc;
gather_unbound_vars([Var=#var{is_ground = IG,
			 unbound_vars= UV,
			 bind = Bind}|Vars],Acc) ->
    if 
	IG ->
	    gather_unbound_vars(Vars,Acc);
	true->
	    case Bind of
		?UNBOUNDVAR ->
		    gather_unbound_vars(Vars,[Var|Acc]);
		_ ->
		    gather_unbound_vars(Vars,Acc++UV)
	    end
    end.





%%TODO: OMIT ONE!!
find(Bindings,ID) ->
%    io:format("[VAR:FIND] Bindings: ~p~nID:~p~n",[Bindings,ID]),
    get_var_from_timestamp(ID,Bindings).

get_var_from_timestamp(TimeStamp,[]) ->
    io:format("[variables.erl] Warning: no variable with timestamp ~p found,\n",
	      [TimeStamp]),
    %exit(avalor),
    [];
get_var_from_timestamp(TimeStamp,[Binding|Bindings]) ->
    case Binding#var.timestamp of
	TimeStamp ->
	    Binding;
	_->
	    get_var_from_timestamp(TimeStamp, Bindings)
    end.
	

%% Valuates a list of params using the bindings
valuate_params(Params,Bindings)->
%    io:format("VALUATE Params: ~p~n",[Params]),
 %   io:format("with Bindings: ~p~n",[Bindings]),
    Fun = fun (X) ->
		  variables:valuate_param(X,Bindings) end,
    lists:map(Fun,Params).

%% Valuates a list of annotations using the bindings
valuate_annots(Annots,Bindings)->
%    io:format("VALUATE Annots: ~p~n",[Annots]),
 %   io:format("with Bindings: ~p~n",[Bindings]),
    Fun = fun (X) ->
		  variables:valuate_annot(X,Bindings) end,
    lists:map(Fun,Annots).




%% Finds the proper valuation for a given param
%valuate_param(Var = #var{is_ground = true},_Bindings)->
%    Var;

valuate_param(V = #var{timestamp = TS, 
		       bind = {TS1,TS2,TS3}, name = Name},Bindings) 
  when is_integer(TS1),is_integer(TS2),is_integer(TS3)  ->
  % Sometimes, the bind is not a timestamp, so more cases are considered
    BTimeStamp = {TS1,TS2,TS3},
%    io:format("Var: ~p~n",[V]),
  %  io:format("TS: ~p~n",[BTimeStamp]),

    
    VarBinding = get_var_from_timestamp(BTimeStamp,Bindings),
 % io:format("VarBinding: ~p~n",[VarBinding]),
    NewBinding = 
	case VarBinding#var.bind of
	    {PName,{},PAnnot} when is_tuple(PName), is_list(PAnnot) ->
		PName;
	    {PName,PArgs,PAnnot} when is_tuple(PName),
				      is_tuple(PArgs),
				      is_list(PAnnot)->
		{valuate_param(PName,Bindings),
		 list_to_tuple(valuate_params(tuple_to_list(PArgs),Bindings)),
		 PAnnot};	        
	    Else->
  %  io:format("Else: ~p~n",[Else]),
		Else

	end,
 %   io:format("NewBinding: ~p~n",[NewBinding]),

    VarBinding#var{timestamp = TS, name = Name,
		  bind = NewBinding};
valuate_param(V = #var{bind = Bind},Bindings)->
	      V;       % This param is already bound
valuate_param(TimeStamp = {_,_,_}, Bindings) ->
   %io:format("TimeStamp: ~p~n",[TimeStamp]),
    valuate_param(#var{bind = TimeStamp},
		  Bindings);
valuate_param(Term,Bindings) -> %% Already bound
    %% io:format("Term: ~p is already bound~n",[Term]),
    Term.

valuate_annot(V = 
	      #var{timestamp = TS,
		   bind = Atom},Bindings) 
  when is_atom(Atom);is_number(Atom)-> %% V is bound from the calling function
    V;


valuate_annot(V = #var{timestamp = TS, 
		       bind = {TS1,TupleTS,ListTS}},Bindings) 
  when is_tuple(TupleTS),is_list(ListTS) ->
%    io:format("Var2: ~p~n",[V]),

%    [Name] = valuate([TS1],Bindings),
    [Variables] = 
	valuate([{TS1,TupleTS,ListTS}],Bindings),

	%{Name,
	%	 list_to_tuple(valuate(tuple_to_list(TupleTS),Bindings)),
	%	 valuate(ListTS,Bindings)},
%    io:format("Variables: ~p~n",[Variables]),
    Bind = fully_valuate(Variables),
    V#var{bind = Bind}.




%% List = [timestamp()]
%% Bindings = [var()] 

%% Returns a list of variables from a list of tuples than include timestamps 
valuate(TSList,Bindings)->
 %   io:format("TSLIst: ~p~n",[TSList]),
    Fun = fun(X) ->
%	io:format("X: ~p~n",[X]),
		  case X of
		      {TS1,TS2,TS3} when is_integer(TS1),
					 is_integer(TS2),
					 is_integer(TS3)  ->
			  variables:get_var_from_timestamp(X,Bindings);

		      {TS,{},[]}-> %added for annots
			  [Res] = valuate([TS],Bindings),
			  Res;
		      {Name,Args,Annots} -> % Added for annots
			  [ValuateName] = valuate([Name],Bindings),
			  {ValuateName,
			   list_to_tuple(valuate(tuple_to_list(Args),Bindings)),
			   valuate(Annots,Bindings)};
		      Atom when is_atom(Atom)->
			  Atom
		  end
	  end,
    lists:map(Fun,TSList).


%% Updates the bindings list for the variables
%% 1st argument is the binding list to be updated
%% 2nd argument is a list of new bindings
update(Bindings,[])->
    Bindings;
update(Bindings,[Var|Rest]) ->
 %   io:format("Var: ~p~n",[Var]),
    NewBindings = remove_var(Bindings,Var),
    update([Var|NewBindings],Rest).
%update(A,B) ->
 %   io:format("[variables:update,error]A: ~p~nB: ~p~n",[A,B]),
  %  exit({error,io_lib:format("variables:update(~p,~p)",[A,B])}).

%% Removes certain variable from the bindings list.
remove_var([],_)->
    [];
remove_var(Bindings,#var{timestamp=TS}) ->
    lists:filter(fun (X) -> TS =/= X#var.timestamp end,
		 Bindings).


%% Strip a struct of all elements that are not
%% variable records
retain_variables(Atom) when is_atom(Atom) ->
    [];
retain_variables(Var) when is_record(Var,var)->
    Var;
retain_variables(Tuple) when is_tuple(Tuple)->
    retain_variables(tuple_to_list(Tuple));
retain_variables(List) when is_list(List)->
    Fun = fun (X) ->
		  retain_variables(X) end,
    lists:flatten(lists:map(
		    Fun,
		    List)).


%% Gets all the variables in a list that are already bound
get_ground_variables(List) when is_list(List) ->
    Fun = fun (#var{is_ground = IG}) ->
		  IG end,
    lists:filter(Fun,List).
		  


%% Valuates a list of params using its bindings
valuate_return_params(Params,Bindings)->
    Fun = fun (X) ->
		  variables:valuate_param(X,Bindings) end,
  %  io:format("Ret Params: ~p~n",[lists:map(Fun,Params)]),
  %  io:format("Valuated: ~p~n",[fully_valuate(lists:map(Fun,Params))]),
   Fun2 = fun 
	      (Var = #var{bind = Bind})->
		 Var#var{bind= fully_valuate(Bind)} end,
    lists:map(Fun2,lists:map(Fun,Params)).





%% Replaces every variable for its binding, whenever it exists
fully_valuate(List) when is_list(List) ->
    Fun = fun (X) ->
		  fully_valuate(X) end,
    lists:map(Fun,List);
%fully_valuate({Var = #var{bind = ?UNBOUNDVAR},
%	       {},[]})->% Added to match standalone variables in annots
 %   Var;
fully_valuate({PredName,
	      Args,Annots}) when is_tuple(Args), is_list(Annots)->
    Fun = fun (X) ->
		  fully_valuate(X) end,
    {fully_valuate(PredName), 
     TArgs = list_to_tuple(lists:map(Fun,tuple_to_list(Args))),
     lists:map(Fun,Annots)};
fully_valuate(Var = #var{bind = ?UNBOUNDVAR}) ->
    Var;
fully_valuate(#var{bind = Bind}) ->
    fully_valuate(Bind);
fully_valuate(Bind) ->
   % io:format("No further valuation for ~p~n",[Bind]),
    Bind.


retain_unique_vars(Element)->
    VarsList = retain_variables(Element),
    utils:erase_repeated_vars(VarsList).


%% Creates a three-element timestamp tuple from the input.
%% Used to match variables in the annotations when these variables
%% are used like the parameters
make_timestamp_tuple({VarName,Args,Annots})->
    {getTimeStamp(VarName),
     getTimeStamp(Args),
     getTimeStamp(Annots)}.
	    

getTimeStamp(#var{timestamp = TS})->
    TS;
getTimeStamp(Vars) when is_tuple(Vars)->
    list_to_tuple(getTimeStamp(tuple_to_list(Vars)));
getTimeStamp(VarsList) when is_list(VarsList) ->
    Fun = fun(Var) ->
		  getTimeStamp(Var) end,
    lists:map(Fun,VarsList).
