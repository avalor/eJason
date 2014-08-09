-module(belief_base).

-export([start/0,start/1,add/2,find/2,remove/2, remove_add/2, %remove_matching/2,
	query_bb/3, add_rules/2]).
	 %%, match_annotations/2, check_consistency/1]).

-include("include/variables.hrl"). 
-include("include/macros.hrl").
-include("include/ejason.hrl").
-include("include/parser.hrl").

%%%%%
%%
%% REMEMBER: all the beliefs are either structs or atoms 
%%           (not numbers, strings or lists)
%% 
%%%%

-record(belief_base,
	{beliefs = orddict:new(),%% orddict where each belief.id is a timestamp
	rules = orddict:new()}).


-record(rule,
	{name = no_rule_name,
	 trigger = {#var{id = no_trigger_var},
		   no_first_condition},
	 conditions = no_rule_conditions, %% orddict similar to a plan context
	 bindings = no_rule_bindings
	}).
	
-record(belief,
	{var = empty_belief, %% reference to the belief in the bindings
	 bindings = orddict:new(), %% Bindings of the belief in the belief base
	 id = erlang:now()
	 %%TODO: allow beliefs that contain free variables
	}).


-record(query_result,
	{query_var = no_query, %% Var that represents the query
	query_bindings = no_query_bindings,
	result = no_result, %% The result is a belief that matches
	result_bindings = orddict:new() %% Bindings that unify the belief and the query
       }).

start()->
    #belief_base{beliefs = orddict:new(),
		 rules = orddict:new()}.


start(Beliefs) when is_list(Beliefs)-> 
    AddFun =
	fun(Belief,BB) ->
		add(Belief,BB) end,
    lists:foldl(AddFun, start(),Beliefs).




    
%% Finds the beliefs in the belief base that match some query.
%% Returns an iterator for the query_results 
find_belief(Query = #query_result{query_var = QueryRef,
				  query_bindings = QueryBindings},
	    BB)->

  



    %% Function that invokes match_vars with the query and every binding
    MatchFun =
	fun (Belief = #belief{var = BeliefRef,
			      bindings = BeliefBindings}) ->
		Bindings =
		    variables:merge_bindings(QueryBindings,
					     BeliefBindings),
		
		case
		    variables:match_vars(Bindings,
					 QueryRef,
					 BeliefRef) of
		    false -> 
			%% case QueryRef of
			%%     #var{functor= {actual_count}} ->
			%% 	io:format("Does not match: ~p~n",[BeliefRef]);
			%%     _ -> ok
			%% end,
			false;
		    ItNewBindings ->
			%% Function that generates the query result for each unification


			ResultFun =
			    fun  (NewBindings) when is_list(NewBindings) ->
				    %% io:format("MATCHED.~n~n"),
				    %% timer:sleep(3000),

				    
				    Query#query_result {
				      result = Belief,
				      result_bindings = NewBindings}
				    %% (OtherThing) ->
				    %%    io:format("OtherThing: ~p~n",[OtherThing])
			    
			    end,
			
			
			
			Res = iterator:create_iterator_fun(ItNewBindings,ResultFun),

			%% io:format("ItNewBindings: ~p~n",
			%% 	  [iterator:get_all(ItNewBindings)]),
			

			
			%% io:format("Res is: ~p~n",[iterator:get_all(Res)]),
			Res
		

		end
	end,
    
    ItBB =
	iterator:create_iterator_orddict(BB#belief_base.beliefs),
    ItRes =
	iterator:create_iterator_fun(ItBB,MatchFun),
    

    ItRes.








%% Add a new belief to the belief base
%% returns {ok,NewBB,AddedBelief} or {ok,no_change}
add(BeliefVar, %%=  #var{annots = Annots},
    BB) ->
   

 
    %% io:format("Adding belief: ~p~n In BB: ~p~n",[BeliefVar,BB]),
    Replacements = 
	variables:obtain_replacements(
	  "BELIEFBASEVAR"++
	  variables:make_timestamp_string()++
	  "_",
	  0,[BeliefVar]),
    
    NewBeliefVar =
	variables:use_replacements(BeliefVar,Replacements),
    			      


    %% Vars in the new belief that will compose its bindings
    NewBeliefVars =
	variables:vars_to_import(NewBeliefVar),

    %% io:format("Adding NewBeliefVars: ~p~n",[NewBeliefVars]),
    %% timer:sleep(20000),
    


    NewBeliefBindings =
	variables:update(orddict:new(),
			 NewBeliefVars),


    NewBelief =
	#belief{ var = variables:get_var(NewBeliefVar#var.id,
					 NewBeliefBindings),
		 bindings = NewBeliefBindings}, 
      
	
    %% io:format("Adding NewBelief: ~p~n",[NewBelief]),
    
    %% Annots = NewBeliefSingleVar#var.annots,

    %% The annotations are not taken into account for the matching, as they will be merged
    ItFind =  find_belief(#query_result{query_var = 
					(NewBelief#belief.var)#var{annots = []},
    					query_bindings = NewBeliefBindings},
    			  BB),  


    %% Check what happens if there were more than one.
    %% There should only be one matching, as EVERY belief is a ground term.
  
    %% TODO: improve the semantics
    case iterator:first(ItFind) of
    	false -> % No other belief matches
	    %% Erase repeated annotations, or those that match:
	    %% belief[annot1] -> belief[annot1]
	    %% belief[annot1,annot1] -> belief[annot1]

	    AddedBelief =
		case 
		    (NewBelief#belief.var)#var.annots of
		    [] ->
			NewBelief;
		    _->
			{AddedBeliefBindings,
			 AddedBeliefVar} = shrink_annots(NewBeliefBindings,
							   NewBelief#belief.var),
			
			NewBelief#belief{var = 
					 AddedBeliefVar,
					 bindings = AddedBeliefBindings}
		end,

	    %% io:format("Finally added Belief: ~p~n",[AddedBelief]),

    	    {ok, 
	     BB#belief_base{
	       beliefs =orddict:store(AddedBelief#belief.id,
				      AddedBelief,
				      BB#belief_base.beliefs)},
	    
	     variables:valuate(AddedBelief#belief.bindings,
			       AddedBelief#belief.var) 
	    };
    	#query_result{result = MatchingBelief,
		      result_bindings = ResultBindings}  ->
    	    %% io:format("Belief to drop: ~p~n",[SomeBelief]),

	    %% Add only the new annotations, if applicable, and generate
	    %% the proper event (or change nothing)
	    AnnotsInMatching =(MatchingBelief#belief.var)#var.annots,
	    		

		%% 1) Identify the new annotations:
		FilterFun =
		fun (AnnotRef) ->
			case variables:match_annotations(
			       ResultBindings,
			       [AnnotRef],
			       AnnotsInMatching) of
			    false ->
				%% this is a new annotation
				true;
			    Iterator when is_function(Iterator) ->
				case iterator:first(Iterator) of
				    false ->
					%% this is a new annotation
					true;
				    _ ->
					false
				end
			end
		end,
	    
	    NewAnnots =
		lists:filter(FilterFun, (NewBelief#belief.var)#var.annots),
	    
	    %% 2) Generate the proper event
	    case NewAnnots of
		[] ->
		    %% no changes to the belief base
		    {ok,no_change};
		_ ->
		    %% 2.1 add new annotations
		    UpdatedBeliefVar =
			(MatchingBelief#belief.var)#var{annots = 
							AnnotsInMatching++
							NewAnnots},
		  

		    ValuatedUpdatedBeliefVar =
			variables:valuate(
			  ResultBindings,
			  UpdatedBeliefVar),
		    %% 2.2. erase unused bindings
		    
		    FinalVars =
			variables:vars_to_import(ValuatedUpdatedBeliefVar),
		    
		    FinalBeliefBindings =
			variables:update(orddict:new(),
					 FinalVars),
		    
		    
		    UpdatedBelief =
			MatchingBelief#belief{var = UpdatedBeliefVar,
					      bindings = FinalBeliefBindings},
		  
		    %% io:format("NewBelief: ~p~n",[UpdatedBelief]),
		   %% io:format("ResultBindings: ~p~n",[ResultBindings]),

		   {ok,
		    BB#belief_base{
		      beliefs = orddict:store(UpdatedBelief#belief.id,
					      UpdatedBelief,
					      BB#belief_base.beliefs)},
		    variables:valuate(
		      FinalBeliefBindings,
		      (MatchingBelief#belief.var)#var{annots =  NewAnnots})}
	    end
    end.


    	    %% %%TODO: Remove the belief that matches and add the new annotations
    	    %% DropBB = orddict:erase(SomeBelief#belief.id,BB),
    	    %% %% io:format("BB before delete: ~p~n",[BB]),
    	    %% %% io:format("DropBB: ~p~n",[DropBB]),
	    %% NewAnnots =
	    %% 	%% keep only not valuated annots
	    %% 	(SomeBelief#belief.var)#var.annots ++
	    %% 	(NewBelief#belief.var)#var.annots,
	    
	    
	    %% %%Add the annotations
	    %% UpdatedBeliefVar =
	    %% 	(SomeBelief#belief.var)#var{annots = NewAnnots},
	           
	    %% Add the new belief to DropBB, its annots will be shrunk
	    %% add(variables:valuate(ResultBindings,UpdatedBeliefVar),
	    %% 	DropBB)	    
    	    %% {ok,orddict:store(NewBelief#belief.id,
    	    %% 		      NewBelief,
    	    %% 		      DropBB)}
    %% end.



%% %% Two beliefs are equal for addition if their structs can be unified while
%% %% their free variables remain free.
%%     e.g. belief(A,[1,2,B],arg(2)[A]

%% equals_for_addition(

%% Remove all the beliefs that match "NewBelief" and adds NewBelief
%% e.g. -+belief(_).
%% Returns {ok,NewBB,NewEvents}
remove_add(NewBelief, BB)->

    %% Turn arguments into unbound variables
    %% e.g  a(b,c) -> a(_,_)
    NewBeliefFunctor = variables:keep_functor(NewBelief),
    
    {RemoveBeliefEvents, UseBB} = 
	case remove(NewBeliefFunctor,BB) of
	    {ok, no_change}->
		{[],BB};
	    {ok, NewBB,RemovedBeliefs} ->
		%% Fun that generates new events for the
		%% beliefs erased
		NewEventsFun =
		    fun (RemovedBelief) ->
			    #event{type=?REMOVEBELIEF, 
				   body = RemovedBelief}
		    end,
		
		NewEvents = lists:map(NewEventsFun,
					RemovedBeliefs),
		{NewEvents,NewBB}
	end,
    
    %% io:format("Original BB: ~p~n",[BB]),
    %% io:format("Removed Belief: ~p~nId est: ~p~n",[NewBelief,NewBeliefFunctor]),
    %% io:format("Result BB: ~p~n",[UseBB]),
    
    case belief_base:add(NewBelief,UseBB) of
	{ok,FinalBB,AddedBelief} ->
	    %% io:format("NewBB: ~p~n",[NewBB]),
	    NewAddEvent =  #event{type=?ADDBELIEF, 
				  body = AddedBelief},
	
	    FinalEvents = RemoveBeliefEvents ++ [NewAddEvent],
	    {ok, FinalBB,FinalEvents};
	    
	{ok, no_change} ->
	    
	    io:format("RC Error: the matching beliefs should have been removed first\n"),
	    exit(belief_base_remove_add_belief_wrong)
    
    end.





%% Remove all the beliefs that match "BeliefVar"
%% e.g. -belief(_).
%% Returns {ok,no_change}|{ok,NewBB,RemovedBeliefs}
remove(BeliefVar, BB)->
    Replacements = 
	variables:obtain_replacements(
	  "BELIEFBASEVAR"++
	  variables:make_timestamp_string()++
	  "_",
	  1,[BeliefVar]),
    
    NewBeliefVar =
	variables:use_replacements(BeliefVar,Replacements),
    
    %% Vars in the new belief that will compose its bindings
    NewBeliefVars =
	variables:vars_to_import(NewBeliefVar),
    
    NewBeliefBindings =
	variables:update(orddict:new(),
			 NewBeliefVars),
    
    %% io:format("NewBeliefBindings: ~p~n",[NewBeliefBindings]),
    %% io:format("Searching for: ~p~n",[NewBeliefVar#var.id]),
    ItFind =  find_belief(#query_result{query_var = 
					variables:get_var(NewBeliefVar#var.id,
							  NewBeliefBindings),
					query_bindings = NewBeliefBindings}, 
			  BB),  
    
    %% Fun that removes all matching beliefs and accumulates them to create the proper events
    RemoveFun =
	fun (#query_result{result = SomeBelief},
	     {NewBB,RemovedList}) ->
		%%Remove the belief that matches    
		case orddict:find(SomeBelief#belief.id,
				  NewBB#belief_base.beliefs) of
		    {ok,_}->
			%% io:format("SomeBelief to drop: ~p~n",[SomeBelief]),
			DropBB = NewBB#belief_base{
				   beliefs =
				   orddict:erase(SomeBelief#belief.id,
						 NewBB#belief_base.beliefs)},
			#belief{ var = SomeBeliefVar,
				 bindings = SomeBeliefBindings} = SomeBelief,
			%%TODO remove  only the proper annotations
			%% {New BB, NewRemovedList}
			{DropBB, [variables:valuate(SomeBeliefBindings, SomeBeliefVar)|RemovedList]};
		    error ->
			%% The belief was erased already
			{NewBB,RemovedList}
		end
	end,

   

    MatchingBeliefs = 
	iterator:get_all(ItFind),
    
    %% io:format("First: ~p~n",[iterator:first(ItFind)]),


    %% io:format("MatchingBeliefs: ~p~n",[MatchingBeliefs]),
    case MatchingBeliefs of
	[]->
	    {ok,no_change};
	_ ->
	    {FinalBB,BeliefsRemoved} = lists:foldl(RemoveFun, {BB, []}, MatchingBeliefs),
	    {ok,FinalBB,BeliefsRemoved}
    end.



%% Looks for some belief that matches (match_vars) to query.
%% Returns an iterator that returns the valuations that match the query
find(#belief{var = QueryRef, bindings = QueryBindings}, BB)->
    ItResults = find_belief(#query_result{query_var =  QueryRef,
					  query_bindings = QueryBindings},
			    BB),
    %% Function that only returns the result bindings for the query
    BindingsFun =
	fun (#query_result{result_bindings = ResultBindings})->
		[ResultBindings] end,
    iterator:create_iterator_fun(ItResults,BindingsFun). 
   




%% %% Remove all the beliefs that match some pattern
%% remove_matching(BeliefPattern,BB) ->
%%     remove_matching(BeliefPattern,BB,[]).

%% remove_matching(BeliefPattern,BB,RemovedList) ->

%%     %%TODO: increase efficiency
%%     case remove(BeliefPattern,BB) of
%% 	{ok,no_change} ->
%% 	    {ok,BB,RemovedList};
%% 	{ok, NewBB, NewRemoved} ->
%% 	    remove_matching(BeliefPattern,NewBB,[NewRemoved|RemovedList])
%%     end.
	 


%% Returns ALWAYS an iterator [Bindings] (which may be empty)
%% This iterator contains the new bindings that fulfill the query
query_bb(AgentInfo=
	 #agent_info{belief_base = BB},
	 Bindings,Query)->
    %% io:format("Query: ~p~n",[Query]),
   

    ValuatedQuery = 
	variables:valuate(Bindings,Query),

 %%    io:format("Valuatedquery for queryBB: ~p~n",[ValuatedQuery]),
    case ValuatedQuery of
	%% the query is "true"
	#var{functor = true,
	     args = ?ISATOM} ->
	    iterator:create_iterator([Bindings]);
	%% the query is "true[...]"
	#var{functor = #var{id = true},
	     args = {}} ->
	    iterator:create_iterator([Bindings]);
	%% the query is "false"
	#var{functor = false,
	     args = ?ISATOM} ->
	    iterator:create_iterator([]);
	%% the query is "false[...]"
	#var{functor = #var{id = false},
	     args = {}} ->
	    iterator:create_iterator([]);
	
	_ ->


	    Replacements = 
		%% Bindings already contains some vars that start with 
		%% the prefix ejasonquerybb (rules invoking rules)
		variables:obtain_replacements(
		  "EJASONQUERYBB"++
		  variables:make_timestamp_string()++
		  "_",
		  1,[ValuatedQuery]),
	    
	    ReplacedQuery =
		variables:use_replacements(ValuatedQuery, Replacements),



	    QueryVars =
		variables:vars_to_import(ReplacedQuery), 

	    ReplacedQueryBindings =
		variables:update(orddict:new(),
				 QueryVars),

	    {QueryBindings,NewQuery} = 
		variables:correct_structs(ReplacedQueryBindings,
					  ReplacedQuery),
	    
    




	    %%io:format("QUERYBB Bindings: ~p~n",[Bindings]),
	    %%io:format("QUERYBB Args: ~p~n",[Args]),


	    %%io:format("ReceivedQuery: ~p~n",[Query]),
	    
	    
	    %%io:format("[BB] Belief Base: ~p~n",[BB]),

	    %% io:format("[BB] ValuatedQuery:\n ~p~n",[variables:valuate(
	    %% 					   QueryBindings,
	    %% 					   NewQuery)]),

	    %% Function to generate a non-conflicting valuate in the case that
	    %% the query is matched


	    ItBeliefs= 
		find(#belief{var = variables:get_var(NewQuery#var.id,
						     QueryBindings),
			     bindings = QueryBindings},
		     BB),



	    %% io:format("Belief results: ~p~n",[iterator:get_all(ItBeliefs)]),
	    
	    %% ItBeliefs = iterator:create_iterator(BeliefResults),    


	    
	    %%	io:format("Arguments: ~p~n",[Arguments]),

	    ItRules = apply_rules(AgentInfo,QueryBindings,
				  variables:get_var(NewQuery#var.id,
						    QueryBindings)),

	    %% io:format("ItRules devuelve: ~p~nItRules\n",
	    %%[iterator:first(ItRules)]),

	    FinalIterator = iterator:concat(ItBeliefs,ItRules),


	    ReplaceFun =
		fun(ImportBindings) ->
			 %% io:format("ImportBindings: ~p~n",
			 %% 	  [ImportBindings]),
			
			NewBindings =
			    variables:import_new_matchings(
			      Bindings, Replacements,
			      "EJASONVARFROMBELIEF"++
			      variables:make_timestamp_string()++
			      "_",
			      ImportBindings),
			[NewBindings] %% is a list to return all of them together 
		end,

	    ItQueryResult = iterator:create_iterator_fun(FinalIterator,ReplaceFun),



	    %% io:format("FinalIterator: ~p~n",[iterator:firs(FinalIterator)]),
	    %% io:format("FinalIterator: ~p~n",[FinalIterator]),

	    %% The return value is an iterator that provides matches
	    %% for the variables in the query 
	    ItQueryResult
    end.



%% TODO: add a generic rule that allows invoking ALL rules (to handle ?A
%% when is unbound)
apply_rules(AgentInfo = #agent_info{belief_base = BB},
	    QueryBindings,Query) ->    
    
    AgentRules = BB#belief_base.rules,

   
    {RuleName} = Query#var.functor,
    
   
    Rule = orddict:find(RuleName,
			AgentRules),
    
    


    %% io:format("Executing rule: ~p~n",[RuleName]),
    %% io:format("Rules: ~p~n",[AgentRules]),
    %% io:format("Result of looking for subrules: ~p~n",[Rule]),
    
    

  
    ItRules =
	case Rule of
	    error ->
		%% There is no rule with name RuleName
		iterator:create_iterator([]);
	    {ok, SubRules}->

		%% try

		ItFun = iterator:create_iterator(SubRules),
		Fun = fun (#rule{trigger = {Trigger,FirstConditionFun},
				 conditions = Conditions,
				 bindings = RuleBindings})->


			      UseBindings = variables:merge_bindings(
					      RuleBindings,
					      QueryBindings),
			      conditions:trigger(AgentInfo,
						 UseBindings,
						 Conditions,
						 Trigger,
						 Query,
						 FirstConditionFun) end,

		iterator:create_iterator_fun(ItFun,Fun)
	
		%% catch
		%%     error:undef -> %% There is no rule
		%% 	%% io:format("UNDEF~n~n"),
		%% 	iterator:create_iterator([])
		%% end
	end,
    %% io:format("Invoked~n"),
    ItRules.
%%    iterator:create_iterator_fun(ItRules,FunRules).
    

%% Add a new set of rules
add_rules(BB = #belief_base{rules = Rules},
	  NewRules)->

    %% io:format("AddedRules: ~p~n",[OrddictRules]),

    %% List = orddict:to_list(OrddictRules),
    %% ?STOP,

    %% io:format("List: ~p~n",[List]),

    %% NewRules = lists:append([X || {_,X} <-List]),
    
    %% io:format("NewRules: ~p~n",[NewRules]),
    
    Fun = fun(SubRule, Acc) ->
		  add_rule(Acc,SubRule) end,

    NewBB =
	BB#belief_base{
	  rules = lists:foldl(Fun,Rules,NewRules)},
    %% io:format("InitialBB: ~p~n",[NewBB]),
 
    %%io:get_line("Enter:"),
    %% ?STOP,
    NewBB.
 
    

from_parsed_rule_to_rule(#parsed_rule{ name = Name,
				       trigger = Trigger,
				       context = Conditions,
				       bindings = Bindings,
				       first_condition = FirstCondition})->
    
    #rule{trigger = {Trigger,FirstCondition},
	  name = Name,
	  conditions = Conditions,
	  bindings = Bindings}.		 

%% Adds a new rule from a parsed_rule record (either initial rule or
%% parsed after executing .add_rule)
add_rule(Rules, ParsedRule = #parsed_rule{name = Name})->
    NewRule = from_parsed_rule_to_rule(ParsedRule),
    orddict:append(Name, NewRule, Rules).
		   
	 





%% %% %% Receives a valuated var and strips it from ref vars
%% %% %% It is used by add_belief to avoid adding beliefs that
%% %% %% are structs whose functor is a list.
%% erase_refs(#var{functor = Func, args = ?ISREF})->
%%     Func;
%% erase_refs(VarList =#var{functor = {Header,Tail}, 
%% 			 args = ?ISLIST}) ->
%%     NewHeader =
%% 	lists:map(fun erase_refs/1, Header),
%%     NewTail =
%% 	case Tail of
%% 	    [{'[]'}]->
%% 		Tail;
%% 	    _ ->
%% 		lists:map(fun erase_refs/1, Header)
%% 	end,		
    
%%     VarList#var{
%%       functor = {NewHeader,NewTail}      
%%      };
%% erase_refs(Var = #var{args = Args}) when Args == ?UNBOUND;
%% 					 Args == ?UNBOUNDLIST;
%% 					 Args == ?ISATOM->
%%     Var;
%% erase_refs(SNegVar = #var{args = ?STRONGNEG}) ->
%%     SNegVar#var{
%%       functor = erase_refs(SNegVar#var.functor)
%%      };
%% erase_refs(Struct = #var{functor = Func,
%% 		     args = Args,
%% 		      annots = Annots}) ->
%%     NewFunc =
%% 	erase_refs(Func),
%%     NewArgs =
%% 	list_to_tuple(lists:map(fun erase_refs/1, tuple_to_list(Args))),
%%     NewAnnots =
%% 	lists:map(fun erase_refs/1, Annots),
%%     Struct#var{
%%       functor = NewFunc,
%%       args = NewArgs,
%%       annots = NewAnnots
%%      }.





%% Erases annotations that match each other
%% Returns {NewBindings, NewVar}
shrink_annots(Bindings, 
		Var = #var{args = Args,
			   annots = Annots}) when is_tuple(Args) ->
%%    io:format("Shrinking annots: ~p~n",[Annots]),
    MatchedAnnots =
	lists:map(fun({Ref}) -> variables:get_var(Ref,Bindings) end,
		  Annots),

    {NewBindings,NewAnnots} =
	shrink_annots(Bindings,MatchedAnnots,[]),

    %% io:format("Final Bindings after shrink: ~p~n
    
    %%% Erase useless variables
    ValuatedVar =
	variables:valuate(NewBindings,
			  Var#var{annots = NewAnnots}),
    
    NewVars =
	variables:vars_to_import(ValuatedVar),
    
    FinalBindings = 
	variables:update(orddict:new(),
			 NewVars),

    {FinalBindings,variables:get_var(ValuatedVar#var.id,
				     FinalBindings)}.


%% Shrink the annots one at a time
shrink_annots(Bindings,[],Accum)->
    {Bindings,lists:reverse(Accum)};
shrink_annots(Bindings,[Annot|Rest],Accum) ->
    
    {NewBindings,NewAnnot,NewRest} =
	shrink_annot(Bindings,Annot,Rest,[]),
    
    shrink_annots(NewBindings,NewRest,[NewAnnot|Accum]).  


shrink_annot(Bindings,Annot,[],Accum) ->
    %%No more variables that could match
    {Bindings,Annot,lists:reverse(Accum)};
shrink_annot(Bindings, Annot1,[Annot2|Rest],Accum) ->
    Match = variables:match_vars(Bindings,
				   Annot1,Annot2),

    case Match of
	false ->
	    %% The variables do not match, then add Annot2 to accum
	    shrink_annot(Bindings,
			 Annot1, Rest,[Annot2|Accum]);
	ItNewBindings when is_function(ItNewBindings) ->
	    case iterator:first(ItNewBindings) of
		false ->
		    shrink_annot(Bindings,
			 Annot1, Rest,[Annot2|Accum]);
		NewBindings ->
		    
		    %% io:format("BB NewBindings: ~p~n",[NewBindings]),
		    {CorrectedBindings,NewAnnot} =
			variables:correct_structs(NewBindings,Annot1),
		    %% Annot1 and Annot2 are replaced by NewAnnot
		    shrink_annot(CorrectedBindings,
				 NewAnnot,Rest,Accum)
	    end
    end.


		

