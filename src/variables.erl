-module(variables).

-compile(export_all).
-include("include/macros.hrl").
-include("include/parser.hrl").
-include("include/variables.hrl").
-include("include/records.hrl").

-define(ERLTOEJASONVAR, "ERLTOEJASONVAR").

%% Checks whether the leftmost variable/atom can be matched to the
%% rightmost one. (e.g. a(b) matches a(b)[c] 
%%                 but  a(b)[c] does not match a(b)
%%
%% NOTE that annotations are ignored. If used in the belief base, they
%% must be ignored.
%%
%% Note: most of the "unification magic" is done here
%%
%% Returns either "false" or an iterator for the new matchings variables.
%% The return value is an iterator because there can be several possible
%% matching due to the annotations of the variables
match_vars(Bindings,
	   #var{id = ID1},
	   #var{id = ID2}) when ID1 == ID2;
				ID1 == ?UNDERSCORE;
				ID2 == ?UNDERSCORE->
    %%io:format("ID1 : ~p~nID2: ~p~n",[ID1,ID2]),
    iterator:create_iterator([Bindings]);
match_vars(Bindings,
	   Var1= #var{ functor = Func1,
		       args = Args1},
	   Var2=#var{functor = Func2,
		     args = Args2}) ->

    %%  io:format("Bindings: ~p~n",[Bindings]),

      %% io:format("\nVariables:match -> Var1: ~p~n\tVar2: ~p~n",
      %% 		[Var1,Var2]),


    Res =    case {Func1,Func2} of
		 {?NOFUNCTOR,?NOFUNCTOR} when Args1==?UNBOUND,Args2 == ?UNBOUND;
					      Args1==?UNBOUND,Args2 == 
					      ?UNBOUNDLIST;
					      Args1==?UNBOUNDLIST,Args2 == 
					      ?UNBOUNDLIST->
		 
		 %% Var1 and Var2 are unbound whether lists or not
	    NewVar =
		Var1#var{functor = {Var2#var.id}, 
			     args =?ISREF},
	    %io:format("NewVar: ~p~n",[NewVar]),
		 iterator:create_iterator([update(Bindings,
						  [NewVar])]);
%%%%%%

	     {?NOFUNCTOR,?NOFUNCTOR} when Args1==?UNBOUNDLIST,
					  Args2 == ?UNBOUND-> 
		 %% Var1 and Var2 are unbound, but Var1 must be a list
		 %% Therefore, Var2 must be a list as well
		 NewVar1 =
		     Var1#var{functor = {Var2#var.id}, 
			      args =?ISREF},
		 NewVar2 = 
		     Var2#var{args =?UNBOUNDLIST},
		 %% io:format("NewVar1: ~p~n",[NewVar1]),
		 %% io:format("NewVar2: ~p~n",[NewVar2]),
		 iterator:create_iterator([update(Bindings,
						  [NewVar1,NewVar2])]);
%%%%%%



	     {?NOFUNCTOR,_} when Args1 == ?UNBOUND, Args2 == ?ISLIST; 
				 Args1 == ?UNBOUND, Args2 == ?ISATOM;
				 Args1 == ?UNBOUND, Args2 == ?STRONGNEG;
				 Args1 == ?UNBOUNDLIST, Args2 == ?ISLIST;
				 Args1 == ?UNBOUNDLIST, Args2 == ?ISATOM, Func2 == [] -> 
		 %% only Var1 is unbound, Var2 is atom/list/string/strongneg
		 %% Check if Var2 contains Var1
		 
		 case check_contains(Bindings,Var1#var.id,Var2) of
		     true ->
			 %% e.g. A = A[3],  A = [1,A,3]
			 false;
		     false ->
			%% io:format("Not Contained1~n~n"),
			 NewVar =
			     Var1#var{functor = {Var2#var.id}, 
				      args =?ISREF},
			 %% io:format("NewVar: ~p~n",[NewVar]),
			 %% io:format("Bindings: ~p~n",[Bindings]),
			 %% io:format("Updated is: ~p~n",
			 %% 	   [update(Bindings,[NewVar])]),

			 iterator:create_iterator([update(Bindings,
							  [NewVar])])
		 end;
	     	     

%%%%%%
	{?NOFUNCTOR,_} when Args1 == ?UNBOUND,Args2 =/= ?ISREF,
			    Func2 == {Var1#var.id}-> 

%% Var1 is unbound, Var2 is a struct whose functor is Var1
%% e.g. A = A[4]
	    iterator:create_iterator([Bindings]);
  
%%%%%%
	{?NOFUNCTOR,_} when Args1 == ?UNBOUND,Args2 =/= ?ISREF-> 

%% only Var1 is unbound, Var2 is a struct (not a ref then)
%% If Var2 is a ref, the matching is further attempted with the referred var

		 %% Check if Var2 contains Var1
		 
		 case check_contains(Bindings,Var1#var.id,Var2) of
		     true ->
			 %% e.g. A = pred(a,A)
			 false;
		     false ->
			 %%io:format("Not Contained2~n~n"),
			 NewVar =
			     Var1#var{functor = {Var2#var.id}, 
				      args =?ISREF},
			 iterator:create_iterator([update(Bindings,
							  [NewVar])])
		 end;

    
	     
%%%%%%
	     {_,?NOFUNCTOR} when Args2 == ?UNBOUND, Args1 == ?ISLIST; 
				 Args2 == ?UNBOUND,Args1 == ?ISATOM;
				 Args2 == ?UNBOUND,Args1 == ?STRONGNEG;
				 Args2 == ?UNBOUNDLIST, Args1 == ?ISLIST; 
				 Args2 == ?UNBOUNDLIST, Args1 == ?ISATOM, Func1 ==[]-> 

		 %% only Var2 is unbound, Var1 is atom/list/string
		 case check_contains(Bindings,Var2#var.id,Var1) of
		     true ->
			 %% e.g. [1,A,3] = A;
			 false;
		     false ->
			 %% io:format("Not Contained3~n~n"),
			 	 NewVar =
		     Var2#var{functor = {Var1#var.id}, 
			 args =?ISREF},
		 iterator:create_iterator([update(Bindings,
						  [NewVar])])
		 
		 end;
	     

%%%%%%
	     {_,?NOFUNCTOR} when Args2 == ?UNBOUND, 
				 Args1 =/= ?ISREF->% only Var2 is unbound
		 
		 %% only Var2 is unbound, Var1 is a struct (not a ref then)
		 %% If Var1 is a ref, the matching is further attempted with the referred var

		 case check_contains(Bindings,Var2#var.id,Var1) of
		     true ->
			 %% e.g. pred(B,C) = A;
			 false;
		     false ->
			 %% io:format("Not Contained3~n~n"),
			 NewVar =
			     Var2#var{functor = {Var1#var.id}, 
			 args =?ISREF},
			 iterator:create_iterator([update(Bindings,
							  [NewVar])])
		     
		 end;
   
%%%%%%

	{{Ref1}, {Ref2}} when Args1 == ?ISREF,
			      Args2 == ?ISREF-> 
	    %%Fun1 and Fun2 are a reference to a var
		 match_vars(Bindings,
			    get_var(Ref1,Bindings),
			    get_var(Ref2,Bindings));
%%%
	{{Ref1}, _} when Args1 == ?ISREF-> %Fun1 is a reference 

		 match_vars(Bindings,
			    get_var(Ref1,Bindings),Var2);


%%%

	     {_, {Ref2}} when Args2 == ?ISREF-> %Fun2 is a reference 
		 
		 match_vars(Bindings,
			    Var1,get_var(Ref2,Bindings));
	     
	     
%%%%%%
	     
	     {Atom,Atom} when Args1 == ?ISATOM,
			      Args2 == ?ISATOM-> 
		 %%Fun1 and Fun2 are atoms
		 iterator:create_iterator([Bindings]);
	     
%%%%
	{{_Header1,_Tail1}, {_Header2,_Tail2}} 
	     when Args1 == ?ISLIST, Args2 == ?ISLIST-> %Matching two lists
		 match_lists(Bindings,Var1,Var2);


%%%%%%

	     {{Ref1}, {Ref2}} when Args1 == ?STRONGNEG,
				   Args2 == ?STRONGNEG-> 
		 %%Fun1 and Fun2 are strong negations
		 match_vars(Bindings,
			    get_var(Ref1,Bindings),
			    get_var(Ref2,Bindings));
	     
	     
		 
%%%%%%%%%%%%%%%%%%%%%%STRUCTS (the hard part)	




	{{_},{_}} when is_tuple(Args1), is_tuple(Args2),
			      size(Args1) == size(Args2)-> 
	    %%Var1 and Var2 represent full structs 

	 
		 
		 %%These structs can be corrupted(e.g 1[B]), so we correct them
		 %%Note: possible source of inneficiency

		 {CorrectedBindings1,NewVar1} =
		     correct_structs(Bindings,Var1),
		 
		 {Bindings2,NewVar2} =
		     correct_structs(CorrectedBindings1,Var2),

		 #var{functor = NewFunc1,
		      args = NewArgs1,
		      annots = NewAnnots1} = NewVar1,
		 
		 #var{functor = NewFunc2,
		      args = NewArgs2,
		      annots = NewAnnots2} = NewVar2,
		 	 
	       

		 %%	 io:format("FuncVar1: ~p~n",[FuncVar1]),
		 if is_tuple(NewArgs1), is_tuple(NewArgs2),
		    size(NewArgs2) == size(NewArgs1) -> 
			 %% NewVar1 and NewVar2 are full structs with the same number of args
			 %% Then, the code can be reused
			 {Ref1} = NewFunc1,
			 {Ref2} = NewFunc2,		 
			 FuncVar1 =
			     get_var(Ref1,Bindings2),
			 FuncVar2 =
			     get_var(Ref2,Bindings2),
			 case
			     match_vars(Bindings2,FuncVar1,FuncVar2) of
			     false ->
				 false;
			     ItNewBindings ->
				 %% io:format("ItNewBindings: ~p~n",[ItNewBindings]),

				 ArgumentPairs = lists:zip(tuple_to_list(NewArgs1),
							   tuple_to_list(NewArgs2)),
						%io:format("Pairs: ~p~n",
						%	      [ArgumentPairs]),

				 %% This function uses the iterators for the bindings of
				 %% each argument to try matching the next arguments
				 Match = fun (_,false) ->
						 false;
					     ({{Elem1},{Elem2}},ItUseBindings) -> 
						 %% Use the iterator for the next argument
						 FunNextArgs =
						     fun (NextArgBindings) ->
							     match_vars(
							       NextArgBindings,
							       get_var(Elem1,
								       NextArgBindings),
							       get_var(Elem2,
								       NextArgBindings))
						     end,
						 iterator:create_iterator_fun(ItUseBindings,
									      FunNextArgs)
					 end,

				 %% There can be several unifications for the variables in functor+args 
				 ItArgsBindings=lists:foldl(Match,ItNewBindings,
							    ArgumentPairs), 
				 AnnotFun =
				     %% Function that matches the annotations
				     fun (false)->
					     false ;
					 (ArgsBindings) ->
					     match_annotations(
					       ArgsBindings,
					       NewAnnots1,
					       iterator:create_iterator(NewAnnots2))
				     end,
				 iterator:create_iterator_fun(ItArgsBindings,
							      AnnotFun)
			 end;
		    true ->
			 %%NewVar1 and NewVar2 are not two structs with same number of args
			 match_vars(Bindings2,NewVar1,NewVar2)
		 end;

%%%%%%%%%%%%

	     {{_},_} when is_tuple(Args1)->	   
		
		 %%Var1 is a struct e.g.: A[B]
	 
		 %%This struct can be corrupted(e.g 1[B]), so we correct it
		 %% Var2 is also corrected in case it was a struct
		 %%Note: possible source of inneficiency
		 {CorrectedBindings1,NewVar1} =
		     correct_structs(Bindings,Var1),
		 
		 {Bindings2,NewVar2} =
		     correct_structs(CorrectedBindings1,Var2),

		 #var{functor = NewFunc1,
		      args = NewArgs1,
		      annots = NewAnnots1} = NewVar1,
		 
		 #var{functor = NewFunc2,
		      args = NewArgs2,
		      annots = NewAnnots2} = NewVar2,

		 %% io:format("NewVar1: ~p~n",[NewVar1]),
		 %% io:format("NewVar2: ~p~n",[NewVar2]),

		 if is_tuple(NewArgs1)-> 
			 %% NewVar1 is still a struct
			 %% Then, the code can be reused
			 {Ref1} = NewFunc1,
			 case get_var(Ref1,Bindings2) of
			     
                         %%%%% Functor is an unbound variable, its functor and args
                         %%%%% may vary		     
			     UnboundVar =#var{args = ?UNBOUND} ->
				 if
				     NewArgs2 == ?ISATOM, NewAnnots1 == [];
				     %% Var2 is an atom e.g A[] = "123"
				     NewArgs2 == ?ISLIST, NewAnnots1 == [];
				     %% Var2 is a list e.g A[] = [1,2]
				     NewArgs2 == ?STRONGNEG,  NewAnnots1 == [] ->
					 %% Var2 is a negation struct e.g A[] = ~a(b,c)[L],
					 
					 %% Binding the unbound var
					 BoundVar =
					     UnboundVar#var{functor = 
							    {NewVar2#var.id},
							    args = ?ISREF,
							    annots = []},
					 %% Binding the struct to the var
					 FinalVar1 =
					     NewVar1#var{args = ?ISREF,
							 functor = {Ref1},
							 annots = []},

					 NewBindings =
					     update(Bindings2,[BoundVar,
									 FinalVar1]),
					 iterator:create_iterator([NewBindings]);


                                     %%%%%%
				     is_tuple(NewArgs2)->
					 %% Var2 is a (bound) struct: A[B] = c(d,e)[L]
					 %%                           A[B] = c[L]

					 %% The unbound var is matched to the struct Var2 without the annotations
					 %%  e.g. A = c(d,e)
					 BoundVar =
					     UnboundVar#var{functor = NewFunc2, args = NewArgs2,annots = []},

					 %% Var1 is bound to the Struct Var2 plus the annotations of Var1
					 FinalVar1 =
					     NewVar1#var{ functor =  NewFunc2, args = NewArgs2},

					 %% Now, the annotations must be matched,
					 UseBindings =
					     update(Bindings2,[BoundVar,FinalVar1]),
					 
					 match_annotations(
					   UseBindings,
					   NewAnnots1,
					   iterator:create_iterator(NewAnnots2));
                                      %%%%%%

				     true ->
					 %% No more possibilities
					 false
				 
				 end;
			     
			     
                           %%%%%%%%%%%%%%%%%%% Functor is a bound variable and NewVar2 is a struct
			     _ when is_tuple(NewArgs2)->
				 {Ref2} = NewFunc2,

				 case get_var(Ref2,Bindings2) of
                                     %%%%% Functor2 is an unbound variable
                                     %%%%% e.g: a(b,c)[annot] = A[L],
				     UnboundVar =#var{args = ?UNBOUND} ->
					 %% The unbound var is matched to the struct NewVar1 without the annotations
					 %%  e.g. a(b,c)=A
					 BoundVar =
					     UnboundVar#var{functor = NewFunc1, args = NewArgs1,annots = []},

					 %% FinalVar2 is bound to the Struct NewVar1 plus the annotations of Var2
					 FinalVar2 =
					     NewVar2#var{functor =  NewFunc1, args = NewArgs1},
					 
					 %% Now, the annotations must be matched,
					 UseBindings =
					     update(Bindings2,[BoundVar,FinalVar2]),

					 match_annotations(
					   UseBindings,
					   NewAnnots1,
					   iterator:create_iterator(NewAnnots2));
                             %%%%%%
				     BoundVar = #var{} when size(NewArgs1) == size(BoundVar#var.args)->
					 %% The number of args is now equal. Iterate and compare again
					 match_vars(Bindings2,NewVar1,NewVar2);				     
				     
				     _ ->
					 false 
					 %% The number of args is different, otherwise the previous clause
					 %% would've matched
				 end;
			     _ ->
				 false %% no other possibilities left, are there?

			 end;
		    true ->
			 %% NewVar1 is no longer a struct, then iterate
			 match_vars(Bindings2,NewVar1,NewVar2)
		 end;

%%%%%%%%%%%%%%%
	     {_,{_}} when is_tuple(Args2)->	   
		 %%Var2 is a struct while Var1 is not

	 
		 {Bindings2,NewVar2} =
		     correct_structs(Bindings,Var2),

		 #var{functor = NewFunc2,
		      args = NewArgs2} = NewVar2,

		 {Ref2} = NewFunc2,
		 
		 if is_tuple(NewArgs2) ->
			 %% NewVar2 is still a tuple, then reuse code
			 case get_var(Ref2,Bindings2) of
                           %%%%% NewFunctor2 is an unbound variable 
			     UnboundVar =#var{args = ?UNBOUND} ->
				 if
				     %%Note: the necessity of NewAnnot2 ==[] is dropped as
				     %% it is in the right-hand side of the equality
				     Args1 == ?ISATOM;
				     %% Var1 is an atom e.g "123" = A[B]
				     Args1 == ?ISLIST;
				     %% Var1 is a list e.g [1,2] = A[B]
				     Args1 == ?STRONGNEG ->
					 %% Var1 is a negation struct e.g ~a(b,c)[L] = A[B]

					 %% Binding the unbound var
					 BoundVar =
					     UnboundVar#var{functor = 
							    {Var1#var.id},
						    args = ?ISREF,
							    annots = []},
					 %% Binding the struct to the var and dropping its annotations

					 FinalVar2 =
					     NewVar2#var{args = ?ISREF,
							 functor = {Ref2},
							 annots = []},

					 NewBindings =
					     update(Bindings2,[BoundVar,
									FinalVar2]),
					 iterator:create_iterator([NewBindings]);
				     true ->
					 false
				 end;

			     #var{args = ?ISATOM,
				  functor = FunctorAtom}
			     when Args1 == ?ISATOM, NewArgs2 == {}, 
				  Func1==FunctorAtom ->
				 %% Var1 and NewVar2 can be the same atom:
				 %% e.g. a = a[b]
				 %% No new matchings
				 iterator:create_iterator([Bindings2]);
			     _Other ->
				 %% io:format("FuncVar2 is a bound var: ~p~n",
				 %% 	   [Other]),
				 false
			 end;

		    true ->
			 %% Var2 is no longer a struct, then iterate
			 match_vars(Bindings2, Var1,NewVar2)
		 end;
%%%%%%
	_ ->
		 
	    %io:format("Size1: ~p~nSize2: ~p~n",[size(Args1),
	%					size(Args2)]),
		  %% io:format("Vars can never match -> Var1: ~p~n\tVar2: ~p~n",
		  %% 	   [Var1,Var2]),
		 
		 
	    false
    end, %% case {Func1,Func2}
%%    io:format("Result for match_vars: ~p~n",[Res]),
    %% case Res of
    %% 	false ->
    %% 	    io:format("FALSE!~n~n");
    %% 	_ ->
    %% 	    io:format("Match!~n~n")
    %% end,

    Res;
	    
match_vars(_Bindings,P1,P2) ->
     io:format("[variables:match_vars/2, error] \nP1: ~p~nP2: ~p~n",[P1,P2]),
    a = b.   



%% Tries to match all the annotations of Annots1 with those of Annots2
%%
%% Returns either "false" or an iterator for the possible matchings
match_annotations(Bindings,
		  [],
		  _ItAnnots2) ->
    %% ALL ANOTATIONS MATCHED: success!
    iterator:create_iterator([Bindings]);
match_annotations(Bindings, [{Annot1}|Rest],ItAnnots2) ->
     %% io:format("Matching the annotation: ~p~n",[Annot1]),
     %% io:format("In the set ~p~n",[iterator:get_all(ItAnnots2)]),

    Var1 = get_var(Annot1,
		   Bindings),
    MatchAnnot1Fun =
	fun ({AnnotFrom2}) ->
		%% The structs in Annots2 must be corrected 
		%% Note: this is a possible source of inneficiency
		%% Var2 = 
		%%     et_var(AnnotFrom2,Bindings),
		%% {UseBindings, CorrectedVar2} =
		%%     variables:correct_structs(Bindings,Var2),
		%% match_vars(UseBindings, Var1,
		%% 	   CorrectedVar2) end,
		Var2 = get_var(AnnotFrom2,Bindings),
		match_vars(Bindings, Var1,
		 	   Var2) end,
    
    %% This iterators tries to match annot1
    ItMatchesForAnnot1 =
	iterator:create_iterator_fun(ItAnnots2,MatchAnnot1Fun),
    
    MatchRestFun =
	fun (false) -> %%Some annotation not matched
		false;
	    (MatchesForAnnot1) ->
		%% io:format("Annotation1 matched: ~p~n",
		%% 	  [variables:valuate(MatchesForAnnot1,
		%% 	     variables:get_var(Annot1,
		%% 			       MatchesForAnnot1))]),
		match_annotations(MatchesForAnnot1, Rest,ItAnnots2)
	end,
    
    %% If annot1 can be matched, match the rest.
    iterator:create_iterator_fun(ItMatchesForAnnot1,MatchRestFun).
    





   
   

%% Returns a variable without variable references (if possible)
%% and without vars in the functor/args/annots
%% i.e. replaces vars for its corresponding values
get_valuated_var(ID,Bindings)
  when is_atom(ID)->
    Var = get_var(ID,Bindings),
    %io:format("Single var: ~p~n",[Var]),
    get_valuated_var(Var,Bindings);
 
get_valuated_var(Var = #var{functor = Func,
			    args = Args,
			    annots = Annots},
		 Bindings) ->
%    io:format("Valuating var: ~p~n",[Var]),
 %   io:format("{Func: ~p,Args: ~p}~n",[Func,Args]),

    {NewFunc,NewArgs,NewAnnots} =
	case {Func,Args} of
	     {_,?ISATOM} when is_atom(Func)->
	      	{Func,Args,
		lists:map(fun (X) -> get_valuated_var(X,Bindings) end,
			  Annots)};
	    {?NOFUNCTOR,?UNBOUND} -> %unbound variable
		{Func,Args,
		 lists:map(fun (X) -> get_valuated_var(X,Bindings) end,
			   Annots)};
	    
	    {{VarRef}, ?ISREF}-> % Functor is a reference to a variable
		ReferredVar =
		    get_valuated_var(VarRef,Bindings),
	        %io:format("ReferredVar: ~p~n",[ReferredVar]),

		{ReferredVar#var.functor, ReferredVar#var.args,
		ReferredVar#var.annots};
	    {{VarRef}, _ }-> % A structure
		ReferredVar =
		    get_valuated_var(VarRef,Bindings),
		ValuatedArgs =
		    lists:map(fun (X) -> get_valuated_var(X,Bindings) end,
			      tuple_to_list(Args)),
		ValuatedAnnots =
		    lists:map(fun (X) -> get_valuated_var(X,Bindings) end,
			      Annots),
		{ReferredVar,
		 list_to_tuple(ValuatedArgs),
		 ValuatedAnnots}
	end,
    
   ReturnVar =  #var{
     id = Var#var.id, %%CHECK IF THIS IS NOT DANGEROUS, any name should work
     functor = NewFunc,
     args = NewArgs,
     annots = NewAnnots
    },
    
  %  io:format("Returning Var: ~p~n",[ReturnVar]),
    ReturnVar.

	    






%% We must: 1) Change the name of the variables in the params, so that there
%%             are no clashes with those in Bindings.
%%
%%          2) Get the new variables generated and add them to bindings.
%%
%%          3) Identify the correspondence, so that it can be reverted after
%%             the execution of the plan.

%% Replaces all the bindings received with new names
%% Returns a new valuation, and the list of replacements :
%% {NewBindings,Replacements}
replace_bindings(Prefix,Bindings) ->
    %% io:format("Received Bindings: ~p~n",[Bindings]),

    ListBindings = orddict:to_list(Bindings),
    ListValues = [Value || {_key,Value} <- ListBindings],

    Replacements = obtain_replacements(Prefix,length(Bindings),ListValues),

    NewListValues = use_replacements(ListValues,Replacements),
   
    NewBindings =
	update([],NewListValues),
    %% io:format("Replaced Bindings: ~p~n",[NewBindings]),
    %% io:format("Replacements: ~p~n",[Replacements]),

    {NewBindings,Replacements}.

    
		  
		  



%% Obtains the id replacements for the variables in a list of vars
%%
%% Numbers, atoms and '[]' (such that functor =:= id) are 
%% spared the change of name
obtain_replacements(Prefix,Num,VarList) ->
     %% io:format("Replacing: ~p~n",[VarList]),
    %% io:format("REPLACING FROM ~s~p \n",
    %% 	      [Prefix,Num]), 
    
    Result = 
	obtain_replacements(Prefix,Num,VarList,[]),
 %% io:format("Result: ~p~n",[Result]),
    %% io:format("REPLACING To ~s~p \n",
    %% 	      [Prefix,length(
    %% 			lists:filter(FilterFun,Result))+Num-1]),
    Result.

obtain_replacements(_Prefix,_Num,[],Replacements) ->
   %% io:format("Final replacements: ~p~n",[Replacements]),
    %% TODO add this replacement only when there are lists

    orddict:store(
      '[]',
      '[]',
      Replacements);
obtain_replacements(Prefix,Num,[Value|Rest],Replacements) ->
    
    NewReplacements =
	my_replace_vars(Prefix, Num, Value,Replacements),
    obtain_replacements(Prefix,Num,Rest,NewReplacements).


    

%% my_replace_vars(Prefix,Param,Num) when is_integer(Num)->
%%     my_replace_vars(Prefix,Param,Num,[]).


%% Returns a list of NewReplacements= [{VarID, NewVarID}] for each binding
%% It is invoked by obtain_replacements.
%%TODO: use a more efficient way of knowing the amount of new variables
my_replace_vars(_Prefix, _Num,
		Var = #var{id = ID, args = Args, functor = Func},
		Replacements) when Args == ?ISATOM, Func =:= ID;
				   ID == '[]'->    
    %% if 
    %% 	Args == ?ISATOM -> io:format("Atomvar spared: ~p~n",
    %% 				     [Var]);
    %% 	true ->
    %% 	    ok
    %% end,
    %% io:format("rep var: ~p~n",[Var]),
    case orddict:find(ID, Replacements) of
	{ok, _} ->
	    Replacements;
	error ->
	    %% Atoms where Func =:= ID are not changed
	    orddict:store(ID,
			  ID,
			  Replacements)
    end;
my_replace_vars(Prefix, Num,
		Var = #var{id = Id, args = Args},
		Replacements) when  Args == ?ISATOM;
				    Args == ?UNBOUND;
				    Args == ?UNBOUNDLIST->
    
    %% if 
    %% 	Args == ?ISATOM -> io:format("Atomvar renamed: ~p~n",
    %% 				     [Var]);
    %% 	true ->
    %% 	    ok
    %% end,
	    
    %% io:format("rep var: ~p~n",[Var]),

    case orddict:find(Id, Replacements) of
	{ok, _} ->
	    Replacements;
	error ->

    %% Counts how many of the replacements introduce new variables that use 
    %% the prefix

    FilterFun =
	fun ({A,A})  ->
		%% io:format("not counting: ~p~n",[A]),
		false;
	    (_Other) ->
		%% io:format("yes, counting: ~p~n",[Other]),
		true
	end,
	    NewVarID = 
		list_to_atom(Prefix++
			     integer_to_list(
			       length(
				 lists:filter(FilterFun,Replacements))+Num)),
	    
	   
	   	    %% io:format("Adding1 NewVar: {~p,~p}~n",[Id,NewVarID]),

	    %%NewVar = Var#var{id = NewVarID},
	    orddict:store(Id,
			  NewVarID,
			  Replacements)
    end;
my_replace_vars(Prefix, Num,
		Var =#var{id = Id,functor = Func, args = Args},
		Replacements) when  Args == ?ISREF;
				    Args == ?STRONGNEG->
    
    %% io:format("rep var: ~p~n",[Var]),

    case orddict:find(Id, Replacements) of
	{ok, _} ->
	    Replacements;
	error ->
    %% Counts how many of the replacements introduce new variables that use 
    %% the prefix

    FilterFun =
	fun ({A,A})  ->
		%% io:format("not counting: ~p~n",[A]),
		false;
	    (_Other) ->
		%% io:format("yes, counting: ~p~n",[Other]),
		true
	end,
	    NewVarID = 
		list_to_atom(Prefix++
			     integer_to_list(
			       length(
				 lists:filter(FilterFun,Replacements))+Num)),
	    
	    %% io:format("Adding2 NewVar: {~p,~p}~n",[Id,NewVarID]),
	    my_replace_vars(Prefix,Num,
			    Func,
			    orddict:store(Id,
					  NewVarID,
					  Replacements))
    end;
my_replace_vars(_Prefix, _Num,
		?EMPTYLISTVAR,
		Replacements) ->
    %% io:format("rep var: ~p~n",[?EMPTYLISTVAR]),


    Replacements;
my_replace_vars(Prefix, Num,
		Var =#var{id = Id, functor = {Header,Tail}, args = ?ISLIST},
		Replacements) ->

    %% io:format("rep var: ~p~n",[Var]),


    case orddict:find(Id,Replacements) of
	{ok,_} ->
	    Replacements;
	error ->
	    %% Function to apply replacements of a list of vars
	    Fun = fun (X,AccumReplacements) ->			  
			  my_replace_vars(Prefix,Num,X,AccumReplacements) end,

	    HeaderReplacements =
		lists:foldl(Fun,
			    Replacements,
			    Header),
	   %% io:format("ReplacingTail: ~p~n",[Tail]),

	    TailReplacements =
		%% case Tail of
		%%     [{'[]'}]->
		%% 	orddict:store('[]',
		%% 		      '[]',
		%% 		      HeaderReplacements);
		%%     [#var{id = '[]'}]->
		%% 	orddict:store('[]',
		%% 		      '[]',
		%% 		      HeaderReplacements);
		%%     _ ->
			 
		lists:foldl(Fun,
			    HeaderReplacements,
			    Tail),
		%%end,

    %% Counts how many of the replacements introduce new variables that use 
    %% the prefix


    FilterFun =
	fun ({A,A})  ->
		 %% io:format("not counting: ~p~n",[A]),
		false;
	    (_Other) ->
		 %% io:format("yes, counting: ~p~n",[Other]),
		true
	end,
	    NewVarID = list_to_atom(Prefix++
				    integer_to_list(
				      length(
					lists:filter(FilterFun,
						     TailReplacements))+
				      Num)),
	    %% io:format("Adding3 NewVar: {~p,~p}~n",[Id,NewVarID]),
  
	    orddict:store(Id,
			  NewVarID,
			  TailReplacements)
    end;
my_replace_vars(Prefix, Num,
		Var = #var{id = Id, functor = Func, 
			   args = Args, annots = Annots},
		Replacements) when is_tuple(Args)-> 

    %% io:format("rep var: ~p~n",[Var]),

    %% Input Var is a struct
    case orddict:find(Id,Replacements) of
	{ok,_} ->
	    Replacements;
	error ->
	    Fun = fun (X,AccumReplacements) ->			  
			  my_replace_vars(Prefix,Num,X,AccumReplacements) end,
	    
	    ReplacementsFunc =
		lists:foldl(Fun,
			    Replacements,
			    [Func]),
	 %   io:format("ReplacementsFunc: ~p~n",
	%	      [ReplacementsFunc]),
	    
	    ReplacementsArgs =
		lists:foldl(Fun,
			    ReplacementsFunc,
			    tuple_to_list(Args)),
	    %% io:format("ReplacementsArgs: ~p~n",
	    %% 	      [ReplacementsArgs]),



	    ReplacementsAnnots =
		lists:foldl(Fun,
			    ReplacementsArgs,
			    Annots),

    %% Counts how many of the replacements introduce new variables that use 
    %% the prefix
	FilterFun =
		fun ({A,A})  ->
			 %% io:format("not counting: ~p~n",[A]),
			false;
	    (_Other) ->
			 %% io:format("yes, counting: ~p~n",[Other]),
			true
		end,   
	    NewVarID = list_to_atom(Prefix++
				    integer_to_list(
				      length(
					lists:filter(FilterFun,
						     ReplacementsAnnots))+
				      Num)),

	    %% io:format("ReplacementsAnnots: ~p~n",[ReplacementsAnnots]),
	    %% io:format("Adding4 NewVar: {~p,~p}~n",[Id,NewVarID]),
	    
	  %  io:format("ReplacementsAnnots: ~p~n",
	%	      [ReplacementsAnnots]),

	    orddict:store(Id,
			  NewVarID,
			  ReplacementsAnnots)
    end;
%% my_replace_vars(Prefix, Num, #binary_operation{
%% 				left_part = Left,
%% 				right_part = Right},Replacements) ->
%%     %% Binary functions are fully valuated
%%     LeftReplacements =
%% 	my_replace_vars(Prefix,Num,Left, Replacements),
%%     AllReplacements = 
%% 	my_replace_vars(Prefix,Num,Left,LeftReplacements),

%%     AllReplacements;

my_replace_vars(Prefix, Num, {VarRef},Replacements) ->

    %%VarRef is not replaced, as it could be an atom
    Replacements.
    
 %% case orddict:find(VarRef,Replacements) of
    %% 	{ok,_} ->
    %% 	    Replacements;
    %% 	error ->
    %% 	     NewVarID = list_to_atom(Prefix++
    %% 				     integer_to_list(length(Replacements)+
    %% 						    Num)),
	    
    %% 	    orddict:store(VarRef,
    %% 			  NewVarID,
    %% 			  Replacements)
    %% end.



%% Applies the replacement of ids for the variables in a list or a var
%%
%% Returns the replaced var(s)
use_replacements(VarList,Replacements) when is_list(VarList)->
    Fun = fun(Var) ->
		  use_replacements(Var,Replacements) end,
    lists:map(Fun,VarList);
use_replacements(?EMPTYLISTVAR, _Replacements) ->
    ?EMPTYLISTVAR;

use_replacements(Var=#var{id=ID, 
			  functor = Func, args =Args,
			  annots =Annots}, 
		 Replacements) ->
    
    Fun = fun (Vars) ->			  
		  use_replacements(Vars,Replacements) end,

    %%io:format("ID: ~p~nRepl: ~p~n",[ID,Replacements]),
    {ok,NewID} =
	orddict:find(ID,Replacements),

    NewVar =
	case Args of
	    _ when Args == ?UNBOUND;
		   Args == ?ISATOM;
		   Args == ?UNBOUNDLIST->

		Var#var{id = NewID};
	    _ when Args == ?ISREF;
		   Args == ?STRONGNEG->
		%% NewFunc =
		%%     case Func of
		%% 	{Ref} ->
		%% 	    {ok,NewRef} =
		%% 		orddict:find(Ref,Replacements),
		%% 	    NewRef;
		%% 	#var{id = SomeID} ->
		%% 	    {ok,NewRef} =
		%% 		orddict:find(SomeID,Replacements),
		%% 	    NewRef
		%%     end,
		CreatedVar =Var#var{id = NewID,
				    functor = Fun(Func)},
		%% io:format("CreatedVar: ~p~n",[CreatedVar]),
		CreatedVar
		;
		%%[NewVar|Vars];
	    _ when Args == ?ISLIST->
		{Header,Tail} = Func,
		NewHeader =
		   Fun(Header),
		NewTail =
		    %% case Tail of
		    %% 	[{'[]'}]->
		    %% 	    [{'[]'}];
		    %% 	_ ->
			    Fun(Tail),
		    %end,
		%%NewVar =
		    Var#var{id = NewID,
			    functor = {NewHeader,NewTail}};
		%%[NewVar|Vars];
	     _ when is_tuple(Args)->
		NewFunc =
		    Fun(Func),
		NewArgs =
		    list_to_tuple(Fun(tuple_to_list(Args))),
		NewAnnots =
		    Fun(Annots),
		Var#var{id = NewID,
			functor = NewFunc,
			args = NewArgs,
			annots = NewAnnots}
	end,

    NewVar;
use_replacements({VarRef},Replacements) ->
    %% io:format("VarRef: ~p~nRepl: ~p~n",[VarRef,Replacements]),
    
    {ok,NewVarRef} = orddict:find(VarRef,Replacements),
    {NewVarRef}.
%% use_replacements(BO = 
%% 		     #binary_operation{left_part = Left,
%% 				       right_part = Right},
%% 		 Replacements) ->
    
%%     BO#binary_operation{
%%       left_part = use_replacements(Left,Replacements),
%%       right_part = use_replacements(Right, Replacements)}.

		
		


%% Matches the variables in two lists
%% Returns an iterator
match_lists(Bindings, % Two empty lists
	    ?EMPTYLISTVAR,
	    ?EMPTYLISTVAR)->

	    %% #var{args = ?ISLIST,
 	    %% 	 functor = {[],[{'[]'}]}},
 	    %% #var{args = ?ISLIST,
 	    %% 	 functor = {[],[{'[]'}]}})->
    iterator:create_iterator([Bindings]);



%%TODO: try to correct the lists so that it is possible to check when 
%% these lists have different size
match_lists(Bindings, % Lists of different size
	    #var{args = ?ISLIST,
		 functor = {Header1,
			    [{'[]'}]}
		},
	    #var{args = ?ISLIST,
		 functor = {Header2,
			    [{'[]'}]}
		}
	   ) when length(Header1) =/= length(Header2)->
    false;


match_lists(Bindings, % One of the lists is empty
	    ?EMPTYLISTVAR,
	    #var{args = ?ISLIST,
		 functor = {[{LastElement}],
			    [{Tail}]}}
	   )->

    
    case valuate(Bindings,
		 get_var(Tail,Bindings)) of
	?EMPTYLISTVAR ->
	    %% Tail2 must already be the empty list
   	    %%  lastelement can be matched to the emptylist,
	    match_vars(Bindings,?EMPTYLISTVAR,
		       get_var(LastElement,Bindings));
	_ ->
	    false
    end;
    
match_lists(Bindings, % One of the lists is empty
	    #var{args = ?ISLIST,
		 functor = {[{LastElement}],
			    [{Tail}]}},
	    ?EMPTYLISTVAR
	   )->
    
    case valuate(Bindings,
	   get_var(Tail,Bindings)) of
	?EMPTYLISTVAR ->
	    %% Tail2 must already be the empty list
   	    %%  lastelement can be matched to the emptylist,
	    match_vars(Bindings,get_var(LastElement,Bindings),
		       ?EMPTYLISTVAR);
	_ ->
	    false	       
    end;    


match_lists(Bindings,
	    #var{args = ?ISLIST,
		 functor = {Header1,Tail1}},
	    #var{args = ?ISLIST,
		 functor = {Header2,
			    Tail2}})-> %%Comparing elements
    %% io:format("Matchin: ~p and ~p~n",[ Header1++[Tail1],
    %% 				       Header2++[Tail2]]),
				      
    match_elems_in_list( Bindings,
			 Header1++[Tail1],
			 Header2++[Tail2]).


%% Receives a list with the elements of two lists that must be matched.
%% If a match can be found, the bindings are updated. If they cannot, 
%% false is returned

match_elems_in_list(Bindings,
		    [[{ElemInTail1}]],
		    [[{ElemInTail2}]])-> % Matching tails
    
    match_vars(Bindings,
	       get_var(ElemInTail1,Bindings),
	       get_var(ElemInTail2,Bindings));

match_elems_in_list(Bindings,
		    [[{'[]'}]],
		    List2)when length(List2) > 1->
    %% One element is the empty list while the other has at least one 
    %% element in the header.
    false;   

match_elems_in_list(Bindings,
		    [[{Elem1}]],
		    List2)-> 
    %% Matching Tail of List1 with List2

    NewListVarId = list_to_atom(lists:flatten("EJASONLISTVAR"++
				 integer_to_list(length(Bindings)))),
 %   io:format("Splitting list: ~p~n",[List2]),
    {NewHeader,[NewTail]} = lists:split(length(List2)-1,List2),
    NewListVar =
	#var{id = NewListVarId,
	     functor = {NewHeader,NewTail},
	     args = ?ISLIST},
    NewBindings = 
	orddict:store(NewListVarId,
		      NewListVar,
		      Bindings),
    
    case match_vars(NewBindings,
		    NewListVar,
		    get_var(Elem1,NewBindings)) of
	false ->
	    false;
	ItNewNewBindings ->
	    ItNewNewBindings
    end;
	
match_elems_in_list(Bindings,
		    List1,
		    [[{Elem2}]])-> 
    %% Matching Tail of List2 with List1
    match_elems_in_list(Bindings,
			[[{Elem2}]],
			List1);
match_elems_in_list(Bindings,
		    [{Elem1}|Elems1],
		    [{Elem2}|Elems2])->
    % Not in the tail yet
    case match_vars(
	   Bindings,
	   get_var(Elem1,Bindings),
	   get_var(Elem2,Bindings)) of
	false ->
	    false;
	ItNewBindings->
	    MatchFun =
		fun (NewBindings) ->
			match_elems_in_list(NewBindings,
					    Elems1,
					    Elems2) end,
	    iterator:create_iterator_fun(ItNewBindings,
					 MatchFun)
    end.




%% %% Returns a list with all the vars contained inside the input var(s)
%% %% The input var included
%% %% TODO: check difference with jasonNewParser:find_vars
%% %% TODO: check difference with variables:vars_to_import
%% gather_vars([]) ->
%%     [];
%% gather_vars(VarList) when is_list(VarList)->
%%     Fun = fun(Var,Acc) -> gather_vars(Var,Acc) end,
%%     lists:foldl(Fun,[],VarList);
%% gather_vars(Var) ->
%%     gather_vars(Var,[]).

%% gather_vars(Var =#var{args = Args}, 
%% 	    Acc) when Args == ?ISATOM; Args == ?UNBOUND; Args == ?ISREF;
%% 		      Args == ?STRONGNEG; Args == ?UNBOUNDLIST->
    
%%     [Var|Acc];
%% gather_vars(Var = #var{functor = {Header,Tail},
%% 		       args = ?ISLIST},
%% 	   Acc) ->
%%     Fun =
%% 	fun(X) -> gather_vars(X) end,

%%     Vars = lists:flatten(lists:map(Fun, lists:append([Header,Tail]))),
%%     lists:flatten(lists:append([[Var|Acc], Vars]));
%% gather_vars(Var = #var{functor = Func, args = Args, annots = Annots},
%% 	   Acc) -> %% Var is a struct
%%     Fun =
%% 	fun(X) -> gather_vars(X) end,
    
%%     VarsFunc = Fun(Func),
%%     VarsArgs = lists:map(Fun, tuple_to_list(Args)),
%%     VarsAnnots = lists:map(Fun, Annots),
%%     lists:flatten(lists:append([[Var|Acc],VarsFunc,VarsArgs,VarsAnnots])); 
%% gather_vars({_Ref},Acc) ->
%%     Acc;
%% gather_vars({parenthesis,
%% 	    Element},Acc) ->
%%     lists:flatten(lists:append(
%%       [gather_vars(Element),
%%        Acc]));
%% gather_vars(#binary_operation{left_part = BodyLeft,
%% 			       right_part = BodyRight},
%% 	    Acc) ->
%%    lists:flatten( lists:append(
%%       [lists:map( 
%% 	 fun variables:gather_vars/1,   
%% 	 BodyLeft),
%%        lists:map(fun variables:gather_vars/1,
%% 		 BodyRight),
%%       Acc])).
%% %% gather_vars(#predicate{name = Name,
%% %% 		       arguments = Args,annotations = Annot},
%% %% 	   Acc) ->
%% %%     Fun = fun(X) -> gather_vars(X) end,
%% %%     lists:flatten(lists:append(
%% %%       lists:map(Fun,[Name|tuple_to_list(Args)]++Annot))).
	   


 
   
	
    

% Finds a variable in the bindings.
% Returns  - A variable (if found) or {ID} (otherwise) if it is an id.
%          - An atom if the associated variable is just an atom
%          - A number if ID is a number
get_var('[]',_)->
    ?EMPTYLISTVAR;
get_var(ID,Bindings)->
   %%io:format("ID: ~p~n",[ID]),
    case orddict:find(ID,Bindings) of
	{ok,Var}->
	    Var;
	error ->
	    io:format("[variables.erl] Warning: variable ~p not found,\n",
		      [ID]),
	    a=b,
	    {ID}
    end.


%% Updates the bindings list (orddict) for the variables
%% 1st argument is the binding list to be updated
%% 2nd argument is a list of new bindings
%% Returns NewBindings
update(Bindings,[])->
    Bindings;
update(Bindings,[Var|Rest]) ->
    %io:format("Updated Var: ~p~n",[Var]),
    NewBindings = orddict:store(Var#var.id,
				Var,
				Bindings),
    update(NewBindings,Rest).


%% %% Strip a struct of all elements that are not
%% %% variable records
%% retain_variables(Atom) when is_atom(Atom) ->
%%     [];
%% retain_variables(Var) when is_record(Var,var)->
%%     Var;
%% retain_variables(Tuple) when is_tuple(Tuple)->
%%     retain_variables(tuple_to_list(Tuple));
%% retain_variables(List) when is_list(List)->
%%     Fun = fun (X) ->
%% 		  retain_variables(X) end,
%%     lists:flatten(lists:map(
%% 		    Fun,
%% 		    List)).


%% %% Gets all the variables in a list that are already bound
%% get_ground_variables(List) when is_list(List) ->
%%     Fun = fun (#var{is_ground = IG}) ->
%% 		  IG end,
%%     lists:filter(Fun,List).
		  


%% %% Valuates a list of params using its bindings
%% valuate_return_params(Params,Bindings)->
%%     Fun = fun (X) ->
%% 		  variables:valuate_param(X,Bindings) end,
%%   %  io:format("Ret Params: ~p~n",[lists:map(Fun,Params)]),
%%   %  io:format("Valuated: ~p~n",[fully_valuate(lists:map(Fun,Params))]),
%%     Fun2 = fun (Var = #var{})->
%% 		   fully_valuate(Var) end,
%%     lists:map(Fun2,lists:map(Fun,Params)).




%% %% Replaces every variable for its binding, whenever it exists
%% fully_valuate(List) when is_list(List) ->
%%     Fun = fun (X) ->
%% 		  fully_valuate(X) end,
%%     lists:map(Fun,List);
%% %fully_valuate({Var = #var{bind = ?UNBOUND},
%% %	       {},[]})->% Added to match standalone variables in annots
%%  %   Var;
%% fully_valuate({PredName,
%% 	      Args,Annots}) when is_tuple(Args), is_list(Annots)->
%%     Fun = fun (X) ->
%% 		  fully_valuate(X) end,
%%     {fully_valuate(PredName), 
%%      TArgs = list_to_tuple(lists:map(Fun,tuple_to_list(Args))),
%%      lists:map(Fun,Annots)};
%% fully_valuate({VarName}) ->
%%     VarName;
%% fully_valuate(Var = #var{id = ID, args = ?UNBOUND}) ->
%%     ID;
%% fully_valuate(Var = #var{id = ID, args = ?UNBOUNDLIST}) ->
%%     ID;
%% %% fully_valuate(Bindings,{PredName, Args,Annot}= Pred) 
%% %%   when is_tuple(PredName),
%% %%        is_tuple(Args),
%% %%        is_list(Annot)
%% %%        ->
%% %%     Val =  {fully_valuate(Bindings,PredName),
%% %% 	    list_to_tuple(fully_valuate(Bindings,tuple_to_list(Args))),
%% %% 	    fully_valuate(Bindings,Annot)},
%% %% 	%    io:format("We evaluate Pred: ~p~nas ~p~n",[Pred,Val]),
%% %%     Val;
%% fully_valuate(Bind) ->
%%    % io:format("No further valuation for ~p~n",[Bind]),
%%     Bind.


%% %% retain_unique_vars(Element)->
%% %%     VarsList = retain_variables(Element),
%% %%     utils:erase_repeated_vars(VarsList).


%% %% Creates a three-element timestamp tuple from the input.
%% %% Used to match variables in the annotations when these variables
%% %% are used like the parameters
%% %% make_timestamp_tuple({VarName,Args,Annots})->
%% %%     {getTimeStamp(VarName),
%% %%      getTimeStamp(Args),
%% %%      getTimeStamp(Annots)}.
	    

%% %% getTimeStamp(#var{timestamp = TS})->
%% %%     TS;
%% %% getTimeStamp(Vars) when is_tuple(Vars)->
%% %%     list_to_tuple(getTimeStamp(tuple_to_list(Vars)));
%% %% getTimeStamp(VarsList) when is_list(VarsList) ->
%% %%     Fun = fun(Var) ->
%% %% 		  getTimeStamp(Var) end,
%% %%     lists:map(Fun,VarsList).


%% Applies valuate to a list. 
%% Added as an extra function to avoid collision with the valuate 
%% for a string variable
valuate_list(Bindings,List) when is_list(List) ->
     %% io:format("Valuating a real  List: ~p~n",[List]),
    Fun = fun (X) -> valuate(Bindings,X) end,
    lists:map(Fun,List).

%% ;
%% valuate_list(Bindings, Other) ->
%%     exit(kill),
%%     io:format("WARNING variables:valuate/2 should be used instead.~n"),
%%     valuate(Bindings,Other).



%% Replaces the var references for the proper vars (not their values,
%% except in the case of ?ISREF) using the argument "Bindings"
%%
%% When a binary operation is found, it is resolved, which may fail.
%%
%% In the case of lists, it is checked whether the the tail references a list
%% NOTE: badformed lists are not allowed by ejason
valuate(_,[])->
    [];
%% valuate(Bindings,{Functor,Args,Annots}) ->
%%    Ret = {valuate(Bindings,Functor),
%%      list_to_tuple(
%%        valuate_list(Bindings,tuple_to_list(Args))),
%%      valuate_list(Bindings,Annots)},
%%    % io:format("RET: ~p~n",[Ret]),
%%     Ret;
valuate(Bindings,Atom) when is_atom(Atom) ->
    %% io:format("Valuating an atom: ~p~n",[Atom]),

    valuate(Bindings,{Atom});
valuate(Bindings,Number) when is_number(Number) ->
    %% io:format("Valuating a number: ~p~n",[Number]),   
    valuate(Bindings,{Number});
valuate(_,?EMPTYLISTVAR) ->
    ?EMPTYLISTVAR;
valuate(_,{'[]'}) ->
    ?EMPTYLISTVAR;
valuate(Bindings,{VarRef})->
    %% io:format("Valuating a ref: ~p~n",[VarRef]),
				       
    case orddict:find(VarRef,Bindings) of
	{ok,Var}->
	    %% io:format("Bindings for Ref: ~p~n",[Bindings]),
	    %% io:format("ref corresponds to: ~p~n",
	    %% 	      [Var]),
	    valuate(Bindings,Var);
	error ->
	    io:format("[~p DEBUG:] Variable ~p not found ~n",
		      [?MODULE,VarRef]),
	    io:format("in Bindings: ~n~p~n",[Bindings]),
	    a = b		
    end;
valuate(Bindings, List) when is_list(List) ->
    %% io:format("Valuating list?: ~p~n",[List]),
    valuate(Bindings,{List});
valuate(Bindings,Var = #var{functor = Func, args = Args,
			    annots = Annots}) ->
     %% io:format("ValuatingVar: ~p~n",[Var]),
    ValuatedVar = 
	case Args of
	    ?ISATOM ->
		Var;
	    ?UNBOUND ->
		Var;
	    ?UNBOUNDLIST ->
		Var;
	    ?ISREF ->
		valuate(Bindings,Func);
	    ?STRONGNEG ->
		Var#var{
		  functor = valuate(Bindings,Func)};
		%%Var#var{functor = valuate(Bindings,Func)};
	    ?ISLIST ->
		{Header,Tail} = Func,
		NewHeader=
		    valuate_list(Bindings,Header),
		NewTail =
		    case valuate_list(Bindings,Tail) of
			[?EMPTYLISTVAR]->
			    [?EMPTYLISTVAR];
			[VarTail = #var{args = TailArgs}]
			%% the tail of a list must be another list
			when TailArgs == ?ISLIST; TailArgs == ?UNBOUNDLIST ->
			   % io:format("VarTAIL: ~p~n",[VarTail]),

			    [VarTail];
			
			[VarTail = #var{args = TailArgs}] 
			when TailArgs == ?UNBOUND ->
			    
			   % io:format("UnboundVarTAIL: ~p~n",[VarTail]),
			    [VarTail#var{args = ?UNBOUNDLIST}];

			OtherTail ->
			    io:format("OtherTAIL: ~p~n",[OtherTail]),
			  %%  a=b,
			    %% io:format("[WARNING: Improper Tail ~p]~n",
			    %% 	      [ OtherTail]),
			    exit(improper_list)
		    end,
		%%io:format("NEWTAIL: ~p~n~n~n",[NewTail]),
		Var#var{
		  functor = {NewHeader,
			     NewTail},
		  annots = []};

	    %%valuate_list(Bindings,Annots)};
	    _ when is_tuple(Args)->
		Var#var{
		  functor = valuate(Bindings,Func),
		  args = list_to_tuple(
			   valuate_list(Bindings,tuple_to_list(Args))),
		  annots = valuate_list(Bindings,Annots)		  
		 };
	    _ ->
		
		io:format("[~p DEBUG:] Cannot valuate Var ~p ~n",
			  [?MODULE,Var]),
		exit(error)
	end,
    %% io:format("ValuatredVar: ~p~n",[ValuatedVar]),

    ValuatedVar;
valuate(Bindings,  
	BO=#binary_operation{
	      left_part = LeftPart,
	      right_part= RightPart}) ->     
      %% io:format("Valuating Binary: ~p~nBindings:~p~n",[BO,Bindings]),
    Operation =
	BO#binary_operation{
	  left_part =  valuate(Bindings,LeftPart),
	  right_part =
	      case RightPart of
		  no_right_part ->
		      no_right_part;
		  _ ->
		      valuate(Bindings,RightPart)
	      end},
    
    %% case Solution
    Solution =
	operations:resolve(Operation),
    #var{id = Solution,
	 functor = Solution,
	 args = ?ISATOM,
	 annots = []}.

    %% #var{id=list_to_atom("SOLVEDBINARYOPERATION"++make_timestamp_string()),
    %% 	 functor = Solution,
    %% 	 args = ?ISATOM}-





%% %% Turns a variable into its tuple representation.
%% %% Unbound variables are left unmodified
%% var_to_tuple(#var{functor = Func,
%% 		  args = Args,
%% 		  annots = Annots}) ->
%%     {NewFunc,NewArgs} =
%% 	case Args of
%% 	    ?ISATOM ->
%% 		{Func,{}};
%% 	    ?ISLIST -> 
%% 		{var_to_tuple(Func),{}};
%% 	    ?UNBOUND ->
%% 		{Func,{}};
%% 	    ?UNBOUNDLIST ->
%% 		{Func,{}};
%% 	    _ ->
%% 		{var_to_tuple(Func),
%% 		 list_to_tuple(var_to_tuple(tuple_to_list(Args)))}
%% 	end,

%%     {NewFunc,
%%      NewArgs,
%%      var_to_tuple(Annots)};
%% var_to_tuple(List) when is_list(List) ->
%%     Fun = fun (X) -> var_to_tuple(X) end,
%%     lists:map(Fun,List).



%% Structs from plan formulas can be wrong (e.g. A = a[g] and B = A[d])
%% will generate a struct whose functor points to A, a variable
%% that already has annotations, which is problematic
%% This function deletes these problems.
%% It is used in the arguments of the operations
%% ModifiedVar is valuated (i.e. bound variables are replaced)
%%
%% Works with BO as well.
%%
%% A struct like "A = 1[B]", is turned to "A = 1"
%% Returns {NewBindings, ModifiedVar}

correct_structs(Bindings,
		UnvaluatedVar)->
    %% io:format("Correcting Struct: ~p~n",[UnvaluatedVar]),
    ValuatedVar = valuate(Bindings,UnvaluatedVar),
    %% io:format("Valuated Struct: ~p~n",[ValuatedVar]),
    CorrectedVar = correct_structs(ValuatedVar),
    %% io:format("CorrectedVar: ~p~n",[CorrectedVar]),
    
    
    NewVars = lists:flatten(vars_to_import(CorrectedVar)),
    %%gather_vars(CorrectedVar)),
       %% io:format("NewCleanVars: ~p~n",[NewVars]),

    CleanVar = clean_var(CorrectedVar),
%%     io:format("Final Corrected Struct: ~p~n",[CleanVar]),

    NewBindings = update(Bindings,NewVars),
    {NewBindings,
     CleanVar}.


%% This auxiliary function corrects the structs but does not
%% modify the bindings (vars_to_import can be used to obtain the new bindings)
%% It is achieved by correct_structs/2
correct_structs(Var = 
		#var{functor = _Func,
		     args = Args, 
		     annots = _Annots}) when Args == ? ISATOM;
					     Args == ?UNBOUNDLIST;
					     Args == ?UNBOUND->
    %% Atoms cannot have annotations
    Var#var{annots = []};
correct_structs(NegVar = #var{functor = Func,
			      args = ?STRONGNEG}) ->

      %% io:format("NegVar",[NegVar]),
    %% io:format("CorrectFunc: ~p~n",
    %% 	      [correct_structs(Func)]),
    CorrectedVar =
	%% Strongneg refers to struct vars for simplicity
	%% ~ a -> ~ a()[]
	case correct_structs(Func) of
	    AtomVar= #var{args = ?ISATOM, functor = F} when is_atom(F) ->
		NewVarID =
		    list_to_atom("EJASONSTRUCTINNEG"++
				 ?MODULE:make_timestamp_string()),
		NewVar=#var{ id = NewVarID,
			     functor = AtomVar,
			     args = {},
			     annots = []},
		
		NegVar#var{
		  functor = NewVar,
		  annots = []
		 };
	    
	    AtomVar= #var{args = ?ISATOM, id = AtomID}  ->
		%% Atomvar is a string or number: ~1, ~"a"
		%% Changed into an atom: ~1 -> 1, ~"a" -> "a"
		NegVar#var{args = ?ISREF, functor = {AtomID}};
	    StructVar = #var{args = Args} when is_tuple(Args)->
		NegVar#var{
		  functor = StructVar,
		  annots = []
		 };
	    
	    %% Correcting: ~~Var
	    NegVar = #var{args = ?STRONGNEG, 
			  functor = #var{id=NegatedRef}}->
		NegVar#var{
		  functor = {NegatedRef},
		  args = ?ISREF,
		  annots =[]};	    
	    UnboundVar = #var{functor = ?NOFUNCTOR}->
		NegVar;	 	
	    List = #var{args = ?ISLIST, id = ListID}->
		%% Error should not use ~[1,2,3]
		%% replaced by [1,2,3]
		NegVar#var{args = ?ISREF, functor = {ListID},annots =[]}
	end;    


     %% io:format("CorrectedFunc: ~p~n",[CorrectedFunc]),
    

correct_structs(?EMPTYLISTVAR) ->
    ?EMPTYLISTVAR;
correct_structs(StructVar = 
		#var{functor = {Header,Tail},
		     args = ?ISLIST}) ->
    CorrectedHeader =
	lists:map(fun correct_structs/1,
		  Header),
    CorrectedTail =
	%% case Tail of
	%%     [{'[]'}] ->
	%% 	Tail;
	%    _ ->
		lists:map(fun correct_structs/1,
			  Tail),
	%% end,
    %% io:format("CorrectedHeader: ~p~nCorrectedTail: ~p~n",
    %% 	      [CorrectedHeader, CorrectedTail]),


    StructVar#var{
      functor = {CorrectedHeader,
		 CorrectedTail},
      annots = []
      %% Lists cannot have annotations
     };
correct_structs(StructVar = 
		#var{functor = Func,
		     args = Args, 
		     annots = Annots}) when is_tuple(Args)->
     %% io:format("StructVar: ~p~n",[StructVar]),
    case Func of
	#var{args = ?ISATOM} ->
	    CorrectedArgs =
		list_to_tuple(
		  lists:map(fun correct_structs/1,
			    tuple_to_list(Args))),
	    
	    %% io:format("CorrectedAnnots: ~p~n",[CorrectedAnnots]),
	    %% Lists and numbers cannot have annotations
	    NewStruct =
		case Func#var.functor of
		    FunctorNumOrStr when is_number(FunctorNumOrStr), Args =={};
					 is_list(FunctorNumOrStr), Args=={}->
			
			%%The struct is turned into a reference
			StructVar#var{
			  functor = {Func#var.id},
			  args = ?ISREF,
			  annots = []
			 };
		    _ ->
			StructVar#var{functor = Func,
				      annots = lists:map(fun correct_structs/1,
							 Annots),
				      args = CorrectedArgs
				     }
		end,
	    


		%% StructVar#var{functor = Func,
		%%        args = CorrectedArgs,
		%%        annots = CorrectedAnnots
		%%       },
	


    %% io:format("CorrectedStruct: ~p~n",[NewStruct]),
	    NewStruct;
	
        #var{args = FuncArgs} when 
	      FuncArgs == ?UNBOUNDLIST, Args =={};
	      FuncArgs == ?ISLIST, Args == {}->
	    %%e.g. Var = [1,2][L]
	    
	    CorrectedFunc =
		correct_structs(Func),
	    StructVar#var{
	      functor = CorrectedFunc,
	      args = ?ISREF,
	      annots = [] 
	      %% Annots ignored if the functor 	
	      %% an atom (number, string) or a list
	     };


	#var{args = FuncArgs} when FuncArgs == ?UNBOUND, Args == {}->
	    %%If the functor is an unbound variable, nothing changes
	    %% Var = A[L]
	    CorrectedAnnots =
		lists:map(fun correct_structs/1,
			  Annots),
	    StructVar#var{
	      annots = CorrectedAnnots
	     };
		   
	#var{functor = StrongNegFunc,
	     args = ?STRONGNEG,
	     annots = _} ->
	    %% The functor is a strong negation, merge the 
	    %% annotations
	    %% e.g. ~a(b)[c][d] -> ~a(b)[c,d]
	    
	    
	    AnnotsFunc = StrongNegFunc#var.annots,
	    
	    %% io:format("StrongNegFun: ~p~n",
	    %% 	      [StrongNegFunc]),
	    CorrectedAnnots =
		lists:map(fun correct_structs/1,
			  Annots++AnnotsFunc),
	    StructVar#var{
	      functor = StrongNegFunc#var{
			  annots = CorrectedAnnots},
	      args = ?STRONGNEG,
	      annots = []
	     } ;
 
	#var{functor = FuncFunc,
	     args = ArgsFunc,
	     annots = AnnotsFunc} when is_tuple(ArgsFunc),
				       Args == {}->
	    %% The functor is another structure, merge the 
	    %% annotations
	    %% e.g. a(b)[c][d]
	    
	    CorrectedAnnots =
		lists:map(fun correct_structs/1,
			  Annots++AnnotsFunc),
	    StructVar#var{
	      functor = FuncFunc,
	      args = ArgsFunc,
	      annots = CorrectedAnnots
	     }	
    end;
correct_structs(BO = #binary_operation{left_part = BodyLeft,
				       right_part = BodyRight}) ->
%%    io:format("BO: ~p~n",[BO]),
    BO#binary_operation{
      left_part =correct_structs(BodyLeft),
      right_part = case BodyRight of 
		       no_right_part ->
			   no_right_part;
		       _ -> correct_structs(BodyRight)
		   end}.
    




%% Receives a var that is bad formed (e.g. an ?ISREF variable whose
%% functor is a variable A, not a reference to A)
%% Does not guarantee that the variables replaced by their
%% references are already captured in some bindings (orddict)
%% It can be achieved using vars_to_import prior to calling clean_var
clean_var(DirtyVar = #var{args = ?ISREF,
			  functor = #var{id = ID}}) ->
    DirtyVar#var{functor = {ID}};
clean_var(DirtyVar = #var{
	    functor = Functor,
	    args = Args,
	    annots = Annots
	   }) when Args =/= ?ISATOM, Args =/= ?ISLIST, Args =/= ?ISREF,
		   Args =/= ?UNBOUND, Args =/= ?UNBOUNDLIST,
		   Args =/= ?STRONGNEG->

  %%   io:format("DirtyVar: ~p~n",[DirtyVar]),
    NewFunc =
	case Functor of
	    {_} -> %% well-formed functor
		Functor;
	    #var{id = FuncID} ->
		{FuncID}
	end,

    RefFun = 
	fun(#var{id = ID}) ->
		{ID};
	   ({Ref}) ->
		{Ref};
	   (BO = #binary_operation{}) ->
		BO
	end,
   
    NewArgs = 
	list_to_tuple(lists:map(RefFun,tuple_to_list(Args))),
    
    NewAnnots =
	lists:map(RefFun,Annots),

    DirtyVar#var{functor = NewFunc,
		 args = NewArgs,
		 annots = NewAnnots};

clean_var(?EMPTYLISTVAR%% Var =#var{args = ?ISLIST,
	  %% 	    id = '[]'}
	 ) ->
    ?EMPTYLISTVAR;
clean_var(Var =#var{args = ?ISLIST,
		    functor = {Header,Tail}}) ->
    
    %% io:format("[Var] Cleaning List: ~p~n",[Var]),
    RefFun = 
	fun(#var{id = ID}) ->
		{ID};
	   ({Ref}) ->
		{Ref}
	end,
    
    NewHeader =
	lists:map(RefFun, Header),
    
    NewTail =
	lists:map(RefFun, Tail),
    %% io:format("NewHeader: ~p~nNewTail: ~p~n",[NewHeader,NewTail]),
    Var#var{functor = {NewHeader,NewTail}};
clean_var(OtherVar) ->
    OtherVar.


%% Receives a variable/binary_operation and generates the
%% list of bindings that can be extracted from it.
%%
%% Unlike in gather_vars (<-deleted), the variables in structs/lists 
%% are replaced by their references
%% 
%%
%% It is used by the belief_base to identify the new bindings
%% imported. 
%% It is used by the parser to get the variables in an action
vars_to_import(Var = #var{args = Args}) when Args == ?ISATOM;
					     Args == ?UNBOUND;
					     Args == ?UNBOUNDLIST->	      
    [Var];
vars_to_import(Var = #var{args = Args}) when Args == ?ISREF;
					     Args == ?STRONGNEG->	      

    %% io:format("Var: ~p~n",[Var]),   
    RefVar =
	Var#var.functor,
    %% io:format("RefVar: ~p~n",[RefVar]),   
    
    {NewVar,NewVarsFunctor} =
	case RefVar of
	    #var{} -> %% If the reference is a variable, take its values
		{Var#var{functor = {RefVar#var.id}, annots = []},
		 vars_to_import(RefVar)};
	    _ ->
		{Var#var{functor = RefVar, annots = []},
		 []}
	end,
    %% io:format("NewVar: ~p~n",[NewVar]),   

    lists:flatten(lists:append( [ [NewVar],
				  NewVarsFunctor]));

vars_to_import(?EMPTYLISTVAR) ->
    [];
vars_to_import(Var =#var{functor = {Header,Tail}, args = ?ISLIST} ) ->
    %%io:format("Vars_to_import: ~p~n",[Var]),
    FunImport = fun (X) ->
			vars_to_import(X) end,
    FunID =
	fun (#var{id = ID})->
		{ID} end,
    %%io:format("vars_to_import on header\n"),
    VarsHeader = lists:map(FunImport, Header),
    
    VarsTail = %% case Tail of
	       %% 	   [{'[]'}] ->
	       %% 	       [];
	       %% 	   _ ->
		           %%io:format("vars_to_import on tail\n"),
		       lists:map(FunImport, Tail),
	       %end,
    NewHeader =
	lists:map(FunID, Header),
    NewTail = 
	%% case Tail of
	%%     [{'[]'}] ->
	%% 	[{'[]'}];
	%%_ ->
		lists:map(FunID, Tail),
	%% end,
   
    NewVar =
	Var#var{
	  functor = {NewHeader,NewTail}
	 },

    Ret = lists:flatten(lists:append([ [NewVar|VarsHeader], VarsTail])),
   %% io:format("Return: ~p~n",[Ret]),
    Ret;
vars_to_import(Var =#var{functor = Functor, args = Args, annots = Annots} ) ->
       %% io:format("Vars_to_import: ~p~n",[Var]),

    FunImport = fun (X) ->
			
			vars_to_import(X) end,
    FunID =
	fun (#var{id = ID})->
		{ID};
	    (BO=#binary_operation{})->
		BO
	end,
    VarsFunctor = FunImport(Functor),
    %% io:format("vars_to_import on functor: ~p\n",[VarsFunctor]),
  

    VarsArgs =
	%% Remove binary operations
	lists:filter(
	  fun %%( {_}) ->
	      %% 	  true;
	      (#binary_operation{}) ->
		  false;
	      (#var{}) ->
		  true
	  end,
	  lists:flatten(lists:map(FunImport,tuple_to_list(Args)))),

    %% io:format("vars_to_import on args: ~p\n",[VarsArgs]),
    
    VarsAnnots = lists:flatten(lists:map(FunImport, Annots)),
   
    %% io:format("vars_to_import on annots; ~p\n",[VarsAnnots]),

 
    NewFunctor = FunID(Functor),
    NewArgs = list_to_tuple(lists:map(FunID,tuple_to_list(Args))),
    NewAnnots = lists:map(FunID,Annots),
    NewVar =
	Var#var{
	  functor = NewFunctor,
	  args = NewArgs,
	  annots = NewAnnots
	 },
    Ret= lists:flatten(lists:append([ [NewVar|VarsFunctor], VarsArgs,VarsAnnots])),
    
    %%io:format("Return: ~p~n",[Ret]),
    Ret;
vars_to_import(#binary_operation{left_part = Left,
				 right_part = Right})->
    VarsLeft =
	case Left of
	    _ when is_atom(Left)->
		[];
	    _ ->
		vars_to_import(Left)
	end,
    
    VarsRight =
	case Right of
	    _ when is_atom(Right)->
		[];
	    _ ->
		vars_to_import(Right)
	end,
    lists:flatten(
      lists:append([ VarsLeft,VarsRight])).
	   



%% Erlang timestamp function "erlang:timestamp"
%% Used to create unique var names
%% Returns a string of length 18 with a timestamp
%%TODO: move to the module utils
make_timestamp_string()->
    List = tuple_to_list( erlang:timestamp()), 
    [A,B,C] =
	lists:map(fun (Num) -> string:right(integer_to_list(Num), 6, $0) end,
		  List),
    A++B++C.




%% Function to update a valuation with the new matching from a query to the
%% Belief Base or the call of a plan.
%% NOTE: This function is a bottleneck for execution performance. An alternative
%% shall be found to increase the performance of the solution
import_new_matchings(OriginalBindings, FirstReplacements,
		     NewVarsPrefix, ImportBindings)->
    %%io:format("Result from Belief: ~p~n",[BeliefBindings]),

    %% TODO: avoid giving "new" values to the
    %% variables already matched when given as params

    %% We must generate new variables to add to bindings
    %% io:format("First Replacements: ~p~n",[FirstReplacements]),

    %% These are the variables that where given as param
    OriginalVars =
	[X|| {X,_} <- FirstReplacements],

    %% These are the new variables generated for the call	
    UsedVars =
	[{Y} || {_,Y} <- FirstReplacements],


     %% io:format("OriginalVars: ~p~n",[OriginalVars]),
     %% io:format("UsedVars: ~p~n",[UsedVars]),
    
    %% io:format("ImportBindings: ~p~n",[ImportBindings]),
    

    
    %% These are the vars that correspond to those of the
    %% call. We need them, because when we valuate them the ones
    %% that are of type ?ISREF get lost.
     CallVars =
     	[get_var(ID,ImportBindings) ||
     	    {ID} <- UsedVars],
    
      %% io:format("CallVars: ~p~n",[CallVars]),
    
 


    %% %% The variables from the call that "disappear" are added to the list
    %% %% of valuated vars 

    %%link first to last!?ISREF -> ?ISREF -> ?ISREF..
    ReplaceIsRef =
	fun(IsRefVar = #var{args = ?ISREF}) ->
		
		%% io:format("ValuatingVar: ~p~n",[IsRefVar]),
		%% io:format("Using: ~p~n",[ImportBindings]),
		ValVar = valuate(ImportBindings,
					   IsRefVar),
		%% io:format("ValuatedVar: ~p~n",[ValVar]),
		IsRefVar#var{functor = {ValVar#var.id}};
	   (Other) ->
		%% io:format("Why??: ~p~n",Other),
		a = b
	end,

    %% io:format("1\n"),
    ErasedVars =
	[ReplaceIsRef(X) || X <-lists:filter(
				  fun(#var{args = ?ISREF}) ->
					  true;
				     (_) -> false
				  end, CallVars)],
    
        %% io:format("3\n"),
    ValuatedVars = 
	valuate_list(ImportBindings,
			       UsedVars)++ ErasedVars,


    %% io:format("ValuatedVars: ~p~n",[ValuatedVars]),


    %% These are the variables to rename
    %% (the ones from the query/call and those referenced
    %% by them)

     %% io:format("ValuatedVars: ~p~n",[ValuatedVars]),

    VarsToRename = 
	sets:to_list(
	  sets:from_list(
	    lists:flatten(lists:map(
			    fun vars_to_import/1,
			    ValuatedVars)))),			
      %% io:format("VarsToRenamen: ~p~n",[VarsToRename]),
    %% io:format("Renaming from: ~p ~n",
    %% 	       [length(OriginalBindings)]),

    NewRepl =
	obtain_replacements(
	  NewVarsPrefix,
	  1,%% length(OriginalBindings),
	  VarsToRename),
        %% io:format("NewRepls: ~p~n",[NewRepl]),

    RenamedVars =
	use_replacements(VarsToRename,
			 NewRepl),


    %% This function maps the variables in the params
    %% to those of the last replacement
    FinalFun =
	fun(VarID) ->
		{ok,Repl1} =
		    orddict:find(
		      VarID, FirstReplacements),
		  %% io:format("Repl1: ~p~n",[Repl1]),

		{ok,Repl2} =
		    orddict:find(
		      Repl1, NewRepl),

		%% io:format("Repl2: ~p~n",[Repl2]),

		case VarID =:= Repl2 of
		    true->
			%% to avoid self-refs
			get_var(VarID,OriginalBindings);
		    false ->
			#var{id = VarID,
			     args = ?ISREF,
			     functor = {Repl2}}
		end

	end,

    FinalMatches =
	lists:map(FinalFun,
		  OriginalVars),

    %% TODO: for debugging purposes, erase
    %% Checks if some variable is going to be wrongly updated
    CheckFun =
	fun(#var{id = VarID, functor = Func}) when Func =/= VarID->
		case orddict:find(VarID,OriginalBindings) of
		    error ->
			false;
		    _ ->
			true
		end;
	   (_) ->
		false
	end,
   
    Updated = lists:filter(CheckFun, lists:flatten(
				       lists:append(
					 [RenamedVars,
					  FinalMatches]))),
    
    %% case Updated of
    %% 	[] ->
    %% 	    ok;
    %% 	_ ->
    %% 	    io:format("Updated: ~p~nIn: ~p~n",[Updated,OriginalBindings]),
    %% 	    timer:sleep(30000)
    %% end,
    
    

    %% Replace the original bindings with the new ones
    FinalResult =
	update(
	  OriginalBindings,
	  lists:flatten(
	    lists:append(
	      [RenamedVars,
	       FinalMatches]))),		
    %% io:format("Ending with a total of: ~p ~n",
    %% 	       [length(FinalResult)]),

    %% timer:sleep(5000),
     %% io:format("FinalResult: ~p~n",[FinalResult]),
    FinalResult.


%% Merges two valuations.
%% Raises an exception if there is a clash (values for the same var differ)
merge_bindings(Bindings1,Bindings2) ->
    MergeFun =
	fun(_,SameValue,SameValue)->
		SameValue;
	   (Key,Value1,Value2) ->
		io:format(
		  "Error: the valuations have conflicting values for var: \n"++
		  "Key: ~p\nValue1:~p\nValue2:~p\n",
		  [Key,Value1,Value2]),
		exit(valuation_merge_bindings_error)
	end,
    orddict:merge(MergeFun, Bindings1,Bindings2).
    


%% Checks if the variables referred by IDVar2 contain
%% IDVar1 e.g. A = ~pred(B,A). 	
check_contains(_Bindings,IDVar1,{IDVar1}) ->
    true;
check_contains(_Bindings,IDVar1,#var{id = IDVar1}) ->
    true;
check_contains(Bindings,IDVar1,{IDVar2}) ->
    check_contains(Bindings,IDVar1,
		   get_var(IDVar2,Bindings));	        
check_contains(_Bindings,_IDVar1,#var{args = Args}) when Args ==?ISATOM;
							 Args ==?UNBOUND;
							 Args ==?UNBOUNDLIST->
    false;
check_contains(Bindings,IDVar1,#var{functor= Ref,
				    args = ?ISREF}) ->
    check_contains(Bindings, IDVar1, Ref);

check_contains(_,_,?EMPTYLISTVAR) ->
    false;
check_contains(Bindings,IDVar1,#var{functor = 
				    {Header,Tail},
				    args = ?ISLIST}) ->
    FunCond = fun (Var) ->
		      check_contains(Bindings,IDVar1,Var) end,
    
    lists:any(FunCond,Header++Tail);
check_contains(Bindings,IDVar1,#var{functor = Ref,
				    args = ?STRONGNEG}) ->
    check_contains(Bindings, IDVar1, Ref);
check_contains(Bindings,IDVar1,#var{functor = Func,
				    args = Args,
				    annots = Annots}) when is_tuple(Args) ->
    FunCond = fun (Var) ->
		      check_contains(Bindings,IDVar1,Var) end,
    lists:any(FunCond,[Func|tuple_to_list(Args)] ++ Annots).
    



%% Checks whether some term contains free variables or not.
%% The variable is fully valuated.
%% Returns true if the term is ground, or not otherwise
is_ground(  #var{functor = ?NOFUNCTOR}) ->
    false;
is_ground(?EMPTYLISTVAR) ->
    true;
is_ground(#var{args = ?ISATOM}) ->
    true;
is_ground(#var{args = ?STRONGNEG,functor = Func}) ->
    is_ground(Func);
is_ground(#var{functor = {Header,Tail}, args = ?ISLIST}) ->
    NotGroundFun =
	fun(Var)->
		not is_ground(Var) end,
    
    not lists:any(NotGroundFun, Header++Tail);
is_ground(#var{args = Args, functor = Func, annots=Annots}) 
  when  is_tuple(Args)->
    NotGroundFun =
	fun(Var)->
		not is_ground(Var) end,
    
    not lists:any(NotGroundFun, [Func|tuple_to_list(Args)]++Annots).




%%% Turns ejason variables into their erlang equivalent
%%% Structs are turned into 3-element tuples:
%%%            a(b,c)[d,e] -> {a,[b,c],[d,e]}
%%% Unbound vars are turned into 3-element tuples:
%%%             A[b] -> {[],[],[b]}
%%% Strong negation structs are turned into 4-element tuples: 
%%%            ~a(b,c)[d,e] -> {'~',a,[b,c],[d,e]}

ejason_to_erl(?EMPTYLISTVAR)->
    [];
ejason_to_erl(Var = #var{functor = Func, args = ?ISATOM}) ->   
    Func;
ejason_to_erl(#var{functor = StructVar,
		args = ?STRONGNEG}) ->
    StructList =
	['~']++ case ejason_to_erl(StructVar) of
		    {Functor,Args,Annots} ->
			tuple_to_list({Functor,Args,Annots});
		    Atom when is_atom(Atom) ->
			[Atom,[],[]]
		end,
    list_to_tuple(StructList);

ejason_to_erl(V = #var{functor = {[Header],[Tail]},
		   args = ?ISLIST}) ->
    %% io:format("[Variables.erl] VarList = ~p~n",[V]),
    [ejason_to_erl(Header)| 
	case Tail of %% avoid always adding an empty list as last element
	    [?EMPTYLISTVAR] ->
		[];
	    _ ->
		ejason_to_erl(Tail)
	end];
ejason_to_erl(#var{functor = ?NOFUNCTOR, args = ?UNBOUND, annots = Annots}) ->
    Fun = fun (X) -> ejason_to_erl(X) end,
    {[],[],lists:map(Fun, Annots)};
   
ejason_to_erl(#var{functor = ?NOFUNCTOR, args = ?UNBOUNDLIST}) ->
    {[],[],[]};
ejason_to_erl(#var{functor = Func, args = Args,annots = Annots}) ->
    {ejason_to_erl(Func),    
     lists:map(fun ejason_to_erl/1, tuple_to_list(Args)),
     lists:map(fun ejason_to_erl/1, Annots)}.



%%% Turns erlang terms into eJason variables - ONLY ONE VARIABLE!
%% NOTE: Strings are treated like lists.
erl_to_ejason([])->
    ?EMPTYLISTVAR;
erl_to_ejason([LastElem]) ->
    Time = make_timestamp_string(),
    #var{
       id = list_to_atom(?ERLTOEJASONVAR++Time),
       functor =
	   {[erl_to_ejason(LastElem)],
	    [?EMPTYLISTVAR]},
       args = ?ISLIST
      };
erl_to_ejason([Header|Tail]) ->
    Time = make_timestamp_string(),
    ListHeader =
	erl_to_ejason(Header),
    ListTail =
	erl_to_ejason(Tail),
		    
    #var{
       id = list_to_atom(?ERLTOEJASONVAR++Time),
       functor={[ListHeader],[ListTail]},
       args = ?ISLIST
      };
erl_to_ejason(Atom) when is_atom(Atom);
			 is_number(Atom)->
    #var{
       id = Atom,
       functor= Atom,
       args = ?ISATOM,
       annots = []
      };
erl_to_ejason(Other) ->
    io:format("[variables:erl_to_ejason] There is currently no support"++
		  " for the automatic translation of"++
		  " an Erlang term:~n ~p into eJason.~n",[Other]).
	






%% Turn arguments into unbound variables
%% e.g  a(b,c) -> a(_,_)
keep_functor(Var = #var{args = Args}) when is_tuple(Args)->
    ArgsList = tuple_to_list(Args),
    
    UnboundVarsFun = 
	fun(_) ->
		 #var{id = list_to_atom(
			     "UNBOUNDVAR"++make_timestamp_string()),
		      functor = ?NOFUNCTOR,
		      args = ?UNBOUND,
		      annots = []} end,
    NewList = 
	lists:map(UnboundVarsFun, ArgsList),
    
    Var#var{args = list_to_tuple(NewList)}.

  

%%% TODO: create a function that allows the search for determined
%%%  annotations, like: search(Bindings,Annots, "annotation(Value1,_)",
%%%  ["Value1",...,"ValueN"] that returns a list of var matchings 
%%%% [Value1,...,ValueN].

%%% Look for an annotation "container(ContainerName)"
%%% Used by actions:execute(...,create_agent)
%%%         actions:execute(..., send)

%% If none is found, "node()" is returned
find_container_name(Bindings,Annots)->
    %% io:format("Looking for container in: ~p~n",[Annots]),

    ContainerNameVar = #var{id =
			    list_to_atom(
			      "CONTAINERNAMEVAR"++
			      make_timestamp_string()),
			    functor =?NOFUNCTOR,
			    args = ?UNBOUND},

    ContainerAtomVar =
	#var{args = ?ISATOM, 
	     id = container,
	     functor = container},

    ContainerVar =  
	#var{ id =   
	      list_to_atom("CONTAINERVAR"++
			   make_timestamp_string()),
	      functor = {container},
	      args = {{ContainerNameVar#var.id}}},

    UseBindings =
	update(Bindings,[ContainerVar,ContainerAtomVar,
			 ContainerNameVar]),


    %% Match annotations receives a lists of var references
    UseAnnots = lists:map(fun (#var{id = ID}) -> {ID} end,
			  Annots),

    FoundContainerName =
	case   match_annotations(
		 UseBindings,
		 [{ContainerVar#var.id}],
		 iterator:create_iterator(UseAnnots)) of
	    false ->
		node();
	    ItAnnots when is_function(ItAnnots) ->
		case iterator:first(ItAnnots) of
		    false ->
			node();
		    NewBindings ->
			%% container(Name) found. Extract match.
			SuggestedContainerVar =
			    valuate(
			      NewBindings,
			      get_var(ContainerNameVar#var.id,
				      NewBindings)),
			
			%% %% io:format("StructVar: ~p~n",[StructVar]),

			%% #var{functor = #var{id = container},
			%%      args = {SuggestedContainerVar}} = StructVar,
			SuggestedContainerVar	      
		end
	end,

    %% io:format("FoundContainerName: ~p~n",[FoundContainerName]),

    ContainerName =
	case FoundContainerName of
	    %% _ when is_atom(FoundContainerName) ->
	    %%  	FoundContainerName;
	    #var{args = ?ISATOM} ->
	    	%% container(SomeName)
	    	FoundContainerName#var.functor;
	    #var{functor = #var{args = ?ISATOM} }->
	    	%%container(SomeName[morelabels])
	    	(FoundContainerName#var.functor)#var.functor;
	    _ ->
		io:format("[Variables Debug:] Invalid containerName: ~p~n",
			  [FoundContainerName]),
		node()
	end,
    %% io:format("ContainerName: ~p~n",[ContainerName]),
    ContainerName.


%%% Look for annotations "persist(Options) or demonitor(Options)"
%%% Used by actions:execute(..., monitor_agent)

%% If none is found, the equivalent of "persist(any)" is returned
find_monitor_options(_Bindings, ?PERSISTANY)->
    #monitor_options{
       persist_unknown_agent = true,
       persist_created_agent = true,
       persist_dead_agent = true,
       persist_restarted_agent = true,
       persist_revived_agent = true,
       persist_unreachable_agent = true
      };	
find_monitor_options(Bindings, Configuration)->
    ErlConfiguration = ejason_to_erl(Configuration),
    %% io:format("[variables] Configuration: ~p~n",[ErlConfiguration]), 
  
    %% removes anything that is not an atom from the configuration
    Filter = fun ( {Functor,[],_}) -> Functor;
		 (Atom ) when is_atom(Atom)-> Atom;
		 (_) -> []
	     end,
    
    case ErlConfiguration of
	{demonitor,[Persist],_} when Persist == any orelse
				     Persist == [any]->
	    #monitor_options{
	  persist_unknown_agent = false,
	  persist_created_agent = false,
	  persist_dead_agent = false,
	  persist_restarted_agent = false,
	  persist_revived_agent = false,
	  persist_unreachable_agent = false
	 };
	{persist, [Persist], _} when Persist =/= any andalso
				     Persist =/= [any]->
	    PersistList = case Persist of
			      _ when is_atom(Persist) ->
				  [Persist];
			      _ when is_list(Persist) ->
				  lists:map(Filter, Persist)
			  end,
	    
	    #monitor_options{
	  persist_unknown_agent = lists:member(unknown_agent,
					       PersistList),
	  
	  persist_dead_agent = lists:member(dead_agent,
					    PersistList),

	  persist_restarted_agent = lists:member(restarted_agent,
						 PersistList),
	  
	  persist_revived_agent =  lists:member(revived_agent,
						PersistList),
	  
	  persist_unreachable_agent =  lists:member(unreachable_agent,
						    PersistList),
	  persist_created_agent = lists:member(created_agent,
					       PersistList)
	 };
	{demonitor, [Demonitor], _}->
	    
	    DemonitorList = case Demonitor of
			      _ when is_atom(Demonitor) ->
				  [Demonitor];
			      _ when is_list(Demonitor) ->
				  lists:map(Filter, Demonitor)
			  end,
	    #monitor_options{
	  persist_unknown_agent = not lists:member(unknown_agent,
					       DemonitorList),
	  
	  persist_dead_agent = not lists:member(dead_agent,
					    DemonitorList),

	  persist_restarted_agent = not lists:member(restarted_agent,
						 DemonitorList),
	  
	  persist_revived_agent =  not lists:member(revived_agent,
						DemonitorList),
	  
	  persist_unreachable_agent =  not lists:member(unreachable_agent,
						    DemonitorList),
	  persist_created_agent = not lists:member(created_agent,
					       DemonitorList)
	 };
	%%{persist,[any],_} ->
	_ -> %% Any other thing is wrong, therefore ignored

	    find_monitor_options(Bindings,?PERSISTANY)
    end.
   	    

%%% Look for annotations "supervision_policy(Options)"
%%% Used by actions:execute(..., supervise_agents)

%% %% No ping does not use any 
%% find_supervision_options(_Bindings, ?NOPING)->
%%     #monitor_options{
%%        persist_unknown_agent = true,
%%        persist_created_agent = true,
%%        persist_dead_agent = true,
%%        persist_restarted_agent = true,
%%        persist_revived_agent = true,
%%        persist_unreachable_agent = true
%%       };	

find_supervision_options({supervision_policy,
				    [OptionsList],
				    _}) when is_list(OptionsList)->

    %% io:format("[Variables] Received Supervision Options: ~p~n",
    %% 	      [OptionsList]),
  
    %% Looks for some pattern in the list 
    Filter = fun ( {Functor,[],_}) -> Functor;
		 (Atom ) when is_atom(Atom)-> Atom;
		 (_) -> []
	     end,
    

    PreSupervisionPolicy =
	case lists:member(no_ping, OptionsList) of
	    true ->
		%% If no_ping is given, do not test divergence
		#supervision_policy{no_ping = true};
	    false ->
		#supervision_policy{
		   no_ping = false,
		   ping = find_ping_policy(OptionsList),
		   unblock = find_unblock_policy(OptionsList),
		   restart = find_restart_policy(OptionsList)}
	end,
    
    SupervisionPolicy =
	PreSupervisionPolicy#supervision_policy{
	  revival = find_revival_policy(OptionsList),
	  restart_strategy = find_restart_strategy(OptionsList)
	 },
    
    %% io:format("[Variables] Using supervision policy: ~p~n",[SupervisionPolicy]),
    SupervisionPolicy;
find_supervision_options(_Other)->
    io:format("[Variables DEBUG] Default supervision options. Received: ~p~n",
	      [_Other]),
    #supervision_policy{
       ping = #ping_policy{},
       unblock = #unblock_policy{},
       restart = #restart_policy{}
      }.

find_ping_policy([])->
    #ping_policy{};
find_ping_policy([{ping,[Frequency, Time, MaxPings], _}|_])
  when is_integer(Frequency),
       is_integer(Time),
       is_integer(MaxPings)->
    #ping_policy{
       frequency = Frequency,
       time = Time,
       maxpings = MaxPings};
find_ping_policy([_|Rest]) ->
    find_ping_policy(Rest).
	
find_unblock_policy([])->
    #unblock_policy{};
find_unblock_policy([{unblock,[never], _}|_])->
    #unblock_policy{
       time = infinity,
       maxunblocks = 0};
find_unblock_policy([{unblock,[always], _}|_])->
    #unblock_policy{
       time = 0,
       maxunblocks = 1};
find_unblock_policy([{unblock,[MaxUnblocks, Time], _}|_])
  when is_integer(Time),
       is_integer(MaxUnblocks) ->
    #unblock_policy{
       time = Time,
       maxunblocks = MaxUnblocks};
find_unblock_policy([_|Rest]) ->
    find_unblock_policy(Rest).


find_restart_policy([])->
    #restart_policy{};
find_restart_policy([{restart,[never], _}|_])->
    #restart_policy{
       time = infinity,
       maxrestarts = 0};
find_restart_policy([{restart,[always], _}|_])->
    #restart_policy{
       time = 0,
       maxrestarts = 1};
find_restart_policy([{restart,[MaxRestarts, Time], _}|_])
  when is_integer(Time),
       is_integer(MaxRestarts) ->
    #restart_policy{
       time = Time,
       maxrestarts = MaxRestarts};
find_restart_policy([_|Rest]) ->
    find_restart_policy(Rest).


find_revival_policy([])->
    #revival_policy{};
find_revival_policy([{revive,[never], _}|_])->
    #revival_policy{
       time = infinity,
       maxrevivals = 0};
find_revival_policy([{revive,[always], _}|_])->
    #revival_policy{
       time = 0,
       maxrevivals = 1};
find_revival_policy([{revive,[MaxRevive, Time], _}|_])
  when is_integer(Time),
       is_integer(MaxRevive) ->
    #revival_policy{
       time = Time,
       maxrevivals = MaxRevive};
find_revival_policy([_|Rest]) ->
    find_revival_policy(Rest).

find_restart_strategy([])->
    Default = #supervision_policy{},
    Default#supervision_policy.restart_strategy;
find_restart_strategy([{strategy,[Strategy], _}|Rest])
  when is_atom(Strategy) ->
    case lists:member(Strategy, [one_for_one, one_for_all, rest_for_one]) of
	true ->
	    Strategy;
	false ->
	    find_restart_strategy(Rest)
    end;
find_restart_strategy([_|Rest]) ->
    find_restart_strategy(Rest).



   	    

