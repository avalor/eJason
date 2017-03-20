-module(operations).

-compile(export_all).

-include("include/macros.hrl").
-include("include/variables.hrl").
-include("include/parser.hrl").


%%%%%%%%%%%%%%%% OPERATIONS CALLED IN A PLAN BODY


%% Used in the arithmetic operations of the body formulas,
%% the rules and the conditions in the context.
%%
%% Returns either {?FAIL} or {replace_bindings, ItNewBindings}
%% TODO: change ?FAIL by false
operation(OriginalBindings,Operator,Arg1,Arg2) ->
    
	 %% io:format("Original Bindings: ~p~n",[OriginalBindings]),
	 %% io:format("~p Arg1: ~p~n",[Operator,Arg1]),
	 %% io:format("~p Arg2: ~p~n",[Operator,Arg2]),

   try
    {CorrectedBindings1,CorrectedArg1} = 
	variables:correct_structs(OriginalBindings,Arg1),
    
    {Bindings,CorrectedArg2} = 
	variables:correct_structs(CorrectedBindings1,Arg2),

	 %% io:format("CorrectedBindings1: ~p~n",[CorrectedBindings1]),
	 %% io:format("Bindings: ~p~n",[Bindings]),
 
    %% 	   %% io:format("~p Arg1: ~p~n",[Operator,Arg1]),
     	    %% io:format("~p CorrectedArg1: ~p~n",[Operator,CorrectedArg1]),

    %% 	   %% io:format("~p Arg2: ~p~n",[Operator,Arg2]),
     	    %% io:format("~p  CorrectedArg2: ~p~n",[Operator,CorrectedArg2]),


    
    Result =
	case Operator of 
	    rel_assig ->
		%% Special case for rel_assig,
		rel_assig(Bindings,CorrectedArg1,CorrectedArg2);
	    _ ->
		?MODULE:Operator(CorrectedArg1,CorrectedArg2)
	    end,
	
	%% io:format("Result: ~p~n",[Result]),
	
	case Result of 
	    %% #var{id = false} ->
	    %% 	{?FAIL};
	    false->
		{?FAIL};
	    true -> %% The operator is among >, < ....
		{replace_bindings, iterator:create_iterator([Bindings])};   
	    NewBindings when is_list(NewBindings)->
		{replace_bindings,iterator:create_iterator([NewBindings])};
	    ItNewBindings when is_function(ItNewBindings) ->
		{replace_bindings,ItNewBindings}
	end
	
    catch
	exit:{unbound_var,VarName}->
	    %% io:format("Var ~p is unbound. Condition fails.\n",[VarName]),
	    {?FAIL};
	  
	  %% error:function_clause ->
	  %%   io:format("Invalid Operation: ~p ~p ~p~n\n",
	  %% 	      [CorrectedArg1,Operator,CorrectedArg2]),
	  %%   %% timer:sleep(3000),
	  %%   {?FAIL};
	  
	  exit:improper_list ->
	    {?FAIL};
	_:_ ->
	    %%io:format("[Operations WARNING:]
	    %% io:format("[~p] Operation Failure: ~n    ~p ~p ~p ~n",
	    %% 	      [?MODULE, CorrectedArg1, Operator, CorrectedArg2]),
	    {?FAIL}	  
    end.


%% Resolves the value of a binary operation and returns a value (not a var)
resolve(#binary_operation{operator = Op,
			  left_part =Arg1,
			  right_part = Arg2})->
   %% io:format("Resolving: ~p ~p ~p~n",[Arg1,Op,Arg2]),
    Result =
	?MODULE:Op(Arg1,Arg2),
    %% io:format("Result: ~p~n",[Result]),
 
    case Result#var.functor of
	Num when is_number(Num) ->
	    case Num == round(Num) of 
		true -> 
		    round(Num);
		false ->
		    Num
	    end;
	Other ->
	    Other
    end;
		    
resolve(#var{id = ID,functor = ?NOFUNCTOR}) ->
    exit({unbound_var, ID});
resolve(#var{functor = Func, args = ?ISATOM}) ->
    Func.



%%%%%%%%%%%%%%%% ARITHMETIC


parenthesis(Arg1,_)->
    Result =
	resolve(Arg1),
    #var{id = Result, functor = Result, args = ?ISATOM}.

opposite(Arg1,_)->
     Result =
	resolve(Arg1) * (-1),
    #var{id = Result, functor = Result, args = ?ISATOM}.




arith_plus(Arg1,Arg2)->
    %% io:format("Arg1: ~p~n",[Arg1]),
    %% io:format("Arg2: ~p~n",[Arg2]),

    Result = resolve(Arg1) + resolve(Arg2),
    #var{id = Result, functor = Result, args = ?ISATOM}.


arith_minus(Arg1,Arg2)->
    
    %% io:format("Substracting: ~p  - ~p~n",
    %% 	     [resolve(Arg1), resolve(Arg2)]),
    Result = resolve(Arg1) - resolve(Arg2),
    #var{id = Result, functor = Result, args = ?ISATOM}.

arith_mult(Arg1,Arg2)->


    Result = 
	%% case Arg2 of
	%%     #var{} ->
		resolve(Arg1) * resolve(Arg2),
	%%     #binary_operation{left_part = Left} ->
	%% 	MultRes = resolve(Arg1) * resolve(Left),
	%% 	resolve(Arg2#binary_operation{
	%% 		  left_part =#var{id = MultRes,
	%% 				 functor = MultRes,
	%% 				 args = ?ISATOM}})
	%% end,
    
   %% io:format("Substracting: ~p  - ~p~n",
   %% 	     [resolve(Arg1), resolve(Arg2)]),
    #var{id = Result, functor = Result, args = ?ISATOM}.

arith_power(Arg1,Arg2)->

    Result =
	%% case Arg2 of
	%%     #var{} ->
	math:pow(resolve(Arg1), resolve(Arg2)),
    
	    %% #binary_operation{left_part = Left} ->
	    %% 	PowRes = math:pow(resolve(Arg1),resolve(Left)),
	    %% 	resolve(Arg2#binary_operation{
	    %% 		  left_part =#var{id = PowRes,
	    %% 				 functor = PowRes,
	    %% 				 args = ?ISATOM}})
	%% end,	     
    #var{id = Result, functor = Result, args = ?ISATOM}.

arith_slash(Arg1,Arg2)->
    Result = 
	%% case Arg2 of
	%%     #var{} ->
		resolve(Arg1) / resolve(Arg2),
	%%     #binary_operation{left_part = Left} ->
	%% 	SlashRes = resolve(Arg1) / resolve(Left),
	%% 	resolve(Arg2#binary_operation{
	%% 		  left_part =#var{id = SlashRes,
	%% 				   functor = SlashRes,
	%% 				   args = ?ISATOM}})
	%% end,
    #var{id = Result, functor = Result, args = ?ISATOM}.



arith_div(Arg1,Arg2)->

    %% io:format("Arg1: ~p~n",[resolve(Arg1)]),
    %%  io:format("Arg2: ~p~n",[resolve(Arg2)]),

    Result = 
	%% case Arg2 of
	%%     #var{} ->
		round(resolve(Arg1)) div round(resolve(Arg2)),
	%%     #binary_operation{left_part = Left} ->
	%% 	DivRes = resolve(Arg1) div resolve(Left),
	%% 	resolve(Arg2#binary_operation{
	%% 		  left_part =#var{id = DivRes,
	%% 				   functor = DivRes,
	%% 				   args = ?ISATOM}})
	%% end,
    #var{id = Result, functor = Result, args = ?ISATOM}.



arith_mod(Arg1,Arg2)->
    
    Result = 
	%% case Arg2 of
	%%     #var{} ->
		round(resolve(Arg1)) rem round(resolve(Arg2)),
	%%     #binary_operation{left_part = Left} ->
	%% 	ModRes = (resolve(Arg1) rem resolve(Left)),
	%% 	resolve(Arg2#binary_operation{
	%% 		  left_part =#var{id = ModRes,
	%% 				   functor = ModRes,
	%% 				   args = ?ISATOM}})
	%% end,
    #var{id = Result, functor = Result, args = ?ISATOM}.




%%%%%%%%%%%%%%%% Relative expressions

rel_ge(#var{functor = Id1},#var{functor = Id2}) when is_number(Id1),
                                                     is_number(Id2);
						     is_atom(Id1),
						     is_atom(Id2)->
   Id1 >= Id2.

rel_lt(#var{functor = Id1},#var{functor = Id2}) when is_number(Id1),
                                                     is_number(Id2);
						     is_atom(Id1),
						     is_atom(Id2)->
    Id1 < Id2.

rel_le(#var{functor = Id1},#var{functor = Id2}) when is_number(Id1),
                                                     is_number(Id2);
						     is_atom(Id1),
						     is_atom(Id2)->
    Id1 =< Id2.

rel_gt(#var{functor = Id1},#var{functor = Id2}) when is_number(Id1),
                                                     is_number(Id2);
						     is_atom(Id1),
						     is_atom(Id2)->
   % io:format("Id1: ~p~nId2: ~p~n",[Id1,Id2]),
    Id1 > Id2.

rel_eq(#var{functor = Id1},#var{functor = Id2}) when is_number(Id1),
                                                     is_number(Id2);
						     is_atom(Id1),
						     is_atom(Id2)->
    Id1 == Id2.

rel_diff(#var{functor = Id1},#var{functor = Id2}) when is_number(Id1),
                                                     is_number(Id2);
						     is_atom(Id1),
						     is_atom(Id2)->
    Id1 /= Id2.



rel_assig(Bindings,
	  InputVar1,
	  BO= #binary_operation{}) ->
    
    ResultBO =
	case resolve(BO) of
	    Num when is_number(Num) ->
		%% io:format("Bo: ~p~nResolved to: ~p~n",
		%% 	  [BO,Num]),
		
		UseNum =
		    case Num == round(Num) of
			true->
			    round(Num);
			false ->
			    Num
		    end,
		#var{functor =UseNum,
		     id = UseNum,
		     args = ?ISATOM};
	    Bool when Bool == true;
		      Bool == false->
		#var{functor =Bool,
		     id = Bool,
		     args = ?ISATOM}
	end,
    %%TODO: consider other kinds of ResultBO
    %% e.g. A = B = 3+4;
    rel_assig(variables:update(Bindings,
			       [ResultBO]),
	      InputVar1,ResultBO);

rel_assig(Bindings,InputVar1,InputVar2) ->
    %% io:format("RELASSIG BINDINGS:~n~p~n",[Bindings]),

    Var1 = 
	case InputVar1 of
	    {VarRef1} ->
		variables:get_var(VarRef1,Bindings);
	    #var{} ->
		InputVar1
	end,		
    
    Var2 = 
	case InputVar2 of
	    {VarRef2} ->
		variables:get_var(VarRef2,Bindings);
	    #var{} ->
		InputVar2
	end,
        
    %% io:format("operations.rel_assig:\nVar1: ~p~nVar2: ~p~n",[Var1,Var2]),
    
    case variables:match_vars(Bindings,Var1,Var2) of 
	false ->
	    false;
	 
	NewBindings->
%	    io:format("NewBindings: ~p~n",[NewBindings]),
	    NewBindings
    end.







rel_decomp({Name,Args,Annot})->
     #var{id = 'RELDECOMP',
	  functor = [Name,tuple_to_list(Args),Annot],
	  args = {}}.



%%%%%%%%%%%%%%%% Logical expressions
%% log_not()when is_boolean(ID)->
%%     not ID;
log_not(Bindings,{?FAIL})->
    {replace_bindings,iterator:create_iterator([Bindings])};
log_not(_SomeBindings,{replace_bindings,_Bindings}) ->
    {?FAIL}.

 %% log_not(Bindings,BB,Module,Query)->
 %%    ItQuery =
 %% 	belief_base:query_bb(BB,Bindings,Query),
    
 %%    case iterator:first(ItQuery) of
 %% 	false -> %% the query cannot be matched
 %% 	    {replace_bindings,iterator:create_iterator([Bindings])};
 %% 	_ -> %% the query can be matched by at least one valuation
 %% 	     {?FAIL}
 %%    end.
	    



%% log_not(_,A) ->
%%     a = b,
%%     io:format("A: ~p~n",[A]),
%%     {?FAIL}.



