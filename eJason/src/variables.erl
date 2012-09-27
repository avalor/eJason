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


%% Checks whether an input parameter matches the pattern expected
%% The input parameters will be bound to the "known variables"
%% 
try_param_match(Bindings,TimeStamp,Param=#var{})->
    OldVar = get_var_from_timestamp(TimeStamp,Bindings),
%    io:format("Got Param: ~p~nOldVar: ~p~n",[Param,OldVar]),
    Res=
	case match_vars(Param,OldVar) of
	    false->
		exit(param_does_not_match);
	    Match when is_record(Match,var)-> 
						%	    io:format("Match: ~p~n",[Match]),
		NewParam = Param#var{%unbound_vars = OldVar,
			     bind = OldVar#var.timestamp},
		
	    NewVar = Match#var{name = OldVar#var.name,
			       timestamp = TimeStamp},
		NewBB = update(Bindings,[NewVar]),
		{NewParam,NewBB};
	    NewVars when is_list(NewVars)->
						%	    io:format("NewVarsMatched: ~p~n",[NewVars]),
		
	    NewParam = Param#var{%unbound_vars = OldVar,
			 bind = OldVar#var.timestamp},
		NewBB = update(Bindings,NewVars),
		{NewParam,NewBB}
	end,
%    io:format("Try_param_matchRes: ~p~n",[Res]),
    Res;

try_param_match(Bindings,TimeStamp,Param) -> % Param is not a variable 
 %   io:format("Param in try_param_match: ~p~n",[Param]),
    try_param_match(Bindings,TimeStamp,
		    #var{is_ground = true, bind = Param}). 
    

match_vars(Var= #var{bind = Bind},#var{bind = Bind}) ->
   Var;
match_vars(Var1= #var{bind = Bind1},Var2=#var{bind = Bind2}) ->
 %  io:format("Bind1: ~p~nBind2: ~p~n",
%	      [Bind1,Bind2]), 
    case {Bind1,Bind2} of
	{?UNBOUNDVAR,_}->
	    Var2;
	{_,?UNBOUNDVAR} ->
	    Var1;
	{Pred1=  {_Name1,_Args1,_Annot1},
	 Pred2 = {_Name2,_Args2,_Annot2}} ->
	   match_predicates(Pred1,Pred2);
	%TODO: what happens with the annotation?

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
try_annot_matches(Bindings,Annotations,Queries)->
  %  io:format("Annotations: ~p~nQueries: ~p~n",[Annotations,Queries]),
    Iterator = try_annot_match(Bindings,Annotations,Queries),
 %   io:format("Returning: ~p~n",[ iterator:first(Iterator)]),

    iterator:first(Iterator).


try_annot_match(Bindings,Annotations,[])->
    iterator:create_iterator([Bindings]);
try_annot_match(Bindings,Annotations,[Query|Rest]) ->
    RealQuery = utils:valuate(Bindings,Query),
%    io:format("Real annot query: ~p~n",[RealQuery]),
    Fun = fun (NewBindings) ->
	      % Allow backtracking when matching annotations
		  FinalBindings = update(Bindings,NewBindings),
		  try_annot_match(FinalBindings,Annotations,Rest) end,
    iterator:create_iterator_fun(
      belief_base:match_annotation(Annotations,RealQuery),
      Fun).



%match_predicates( #predicate{name = Name1,arguments= Args1},
%		 #predicate{name = Name2,arguments= Args2})->
 %   case match_vars(Name1,Name2) of
%	false->
%	    false;
%	Name ->
%	    case match_arguments(Args1,Args2) of
%		false ->
%		    false;
%		Args ->
%		    Unbound = gather_unbound_vars([Name|Args]),
%		    #predicate{name=Name,
%			       arguments = Args,
%			       unbound_vars= Unbound,
%			       is_ground = length(Unbound)==0}
%	    end
%   end.

match_predicates( {Name1,Args1,_},
		 {Name2,Args2,_})->
    case match_vars(Name1,Name2) of
	false->
	    false;
	Name ->
	    case match_arguments(tuple_to_list(Args1),
				 tuple_to_list(Args2)) of
		false ->
		    false;
		Args ->
		    %Unbound = gather_unbound_vars([Name|Args]),
		    [Name|Args]
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
    get_var_from_timestamp(ID,Bindings).

get_var_from_timestamp(TimeStamp,[]) ->
    io:format("[variables.erl] Warning: no variable with timestamp ~p found,\n",
	      [TimeStamp]),
    exit(avalor),
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
    Fun = fun (X) ->
		  variables:valuate_param(X,Bindings) end,
    lists:map(Fun,Params).




%% Finds the proper valuation for a given param
%valuate_param(Var = #var{is_ground = true},_Bindings)->
%    Var;
valuate_param(#var{timestamp = TS, bind = BTimeStamp, name = Name},Bindings)->
 %   io:format("TS: ~p~n",[BTimeStamp]),

    VarBinding = get_var_from_timestamp(BTimeStamp,Bindings),
  % io:format("VarBindings: ~p~n",[VarBinding]),
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
%		io:format("Else: ~p~n",[Else]),
		Else

	end,
 %   io:format("NewBinding: ~p~n",[NewBinding]),

    VarBinding#var{timestamp = TS, name = Name,
		  bind = NewBinding};
valuate_param(TimeStamp = {_,_,_}, Bindings) ->
    valuate_param(#var{bind = TimeStamp},
		  Bindings).

%% List = [timestamp()]
%% Bindings = [var()] 

%% Returns a list of variables from a list of timestamps  
valuate(TSList,Bindings)->
    Fun = fun(X) ->
		  variables:get_var_from_timestamp(X,Bindings) end,
    lists:map(Fun,TSList).


%% Updates the bindings list for the variables
%% 1st argument is the binding list to be update
%% 2nd argument is a list of new bindings
update(Bindings,[])->
    Bindings;
update(Bindings,[Var|Rest]) ->
 %   io:format("Var: ~p~n",[Var]),
    NewBindings = remove_var(Bindings,Var),
    update([Var|NewBindings],Rest);
update(A,B) ->
    io:format("[variables:update]A: ~p~nB: ~p~n",[A,B]),
    exit({error,io_lib:format("variables:update(~p,~p)",[A,B])}).

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
fully_valuate({PredName,
	      Args,Annot})->
    Fun = fun (X) ->
		  fully_valuate(X) end,
    {fully_valuate(PredName), 
     TArgs = list_to_tuple(lists:map(Fun,tuple_to_list(Args))),
     Annot};
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


