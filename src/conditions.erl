 
-module(conditions).

-export([trigger/6,
	return/1,
	logical_and/5, logical_or/5,
	strong_negation/5,
	true/4, false/4,
	log_not/5, query_bb/5,
	operation/5, internal_action/6]).

-include("include/variables.hrl").
-include("include/macros.hrl").
-include("include/dm_sm_responses.hrl").
-include("include/dm_responses.hrl").
-include("include/suspendedActionsRecords.hrl").


%%% Executes a condition in a plan context / rule body
%%% Once a condition fails, it invokes the subsequent conditions
%%% but replaces bindings by {?FAIL} (necessary, as that failure may
%%% be intended.
%%% An UNBLOCK signal may stop this iterator
test_condition(AgentInfo,Bindings,Conditions,
	       ExecuteCondition)->

    
     %% io:format("Execute: ~p~n",[ExecuteCondition]),
     %%io:format("Bindings: ~p~n",[Bindings]),

    %% timer:sleep(50),
    %% io:format("From: ~p~n",[Conditions]),****
    receive
	{signal, ?UNBLOCK} ->
	    %% io:format("[Conditions] Agent Unblocked ~n"),
	    %% timer:sleep(100),
%%	    [orddict:new()]
	    {?UNBLOCK}
    after 0 ->
			
	    {ok, {Module,Function,Args}} = orddict:find(ExecuteCondition,
							Conditions),
	    NewArgs = case Function of
			  return ->
			      [Bindings];
			  _ ->
			      [AgentInfo,Bindings,Conditions|Args]
		      end,
	    apply(Module,Function,NewArgs)
    end.
	
		  
	       

%% Returns either false (the trigger+context do not match) or an ItBindings
trigger(AgentInfo,Bindings,Conditions,Trigger,Event,FirstContextFun)->
    case variables:match_vars(Bindings,Trigger,Event) of
     false -> false; %% trigger does not match
     ItTriggerBindings -> 
       Fun = fun (TriggerBindings) ->
		     %% io:format("[Contiditions] TriggerBindings: ~p~n",
		     %% 	       [TriggerBindings]),
		     test_condition(AgentInfo,
				    TriggerBindings,
				    Conditions,
				    FirstContextFun) 
	     end,
       iterator:create_iterator_fun(ItTriggerBindings,Fun)
    end.



%% TODO: include all similar code in a single function

%% Invoked when there are no subsequent conditions to invoke
%% Returns either false or ItBindings.
return(Bindings) ->
    case Bindings of
	{?FAIL} -> 
	    false;
	{?UNBLOCK} ->
	    io:format("[Conditions] Unblock in return function~n"),
	    false;
	_ -> 
	    iterator:create_iterator([Bindings]) 
    end.



%% All these return an iterator that may generate sets of 
%% Bindings (or {?FAIL})



%% Concatenates the results received from the left branch into the
%% right one.
logical_and(AgentInfo,Bindings,Conditions,LeftBranch,RightBranch)->
  %%Condition: logical AND
    %% io:format("LogicalAND: ~n"),

    %% timer:sleep(2000),
    ItRes =   
	test_condition(AgentInfo,Bindings,
		       Conditions,LeftBranch), 
    %% io:format("ITRES: ~p~n",
    %% 	      [ItRes]),
    
    Fun = fun (NewBindings) -> 
		  test_condition(AgentInfo,NewBindings,
				 Conditions,RightBranch)
	  end,
    iterator:create_iterator_fun(ItRes,Fun).


%% Branches the execution to both the left and right children
logical_or(AgentInfo,Bindings,Conditions,LeftBranch,RightBranch)->
  %%Condition: logical OR 
     %% io:format("LogicalOR: ~n"),

     %% timer:sleep(2000),


    FLeft = fun () ->
		     test_condition(AgentInfo,Bindings,
				    Conditions,LeftBranch) 
	     end,
    FRight = fun () ->
		    test_condition(AgentInfo,Bindings,
				   Conditions,RightBranch) 
	    end,
    CallingFun = fun(F) -> 
			 F() end,
    FunIterator = iterator:create_iterator([FLeft,FRight]),
    iterator:create_iterator_fun(FunIterator,CallingFun).


strong_negation(AgentInfo,Bindings,Conditions,QueryVar,NextCondition)->
   
%% Condition: strong negation

    Query = {strong_neg,QueryVar},

    ItRes = 
	case Bindings of 
	    {?UNBLOCK} ->
		io:format("[Conditions] Unblock in StrNeg function~n"),
		{?UNBLOCK};

	    {?FAIL} -> 
		iterator:create_iterator([{?FAIL}]);
	    _ -> 
		case belief_base:query_bb(AgentInfo,Bindings,Query) of
		    false -> 
			iterator:create_iterator([{?FAIL}]);
		    It when is_function(It) -> 
			It 
		end 
	end,

    Fun = fun (NewBindings) ->
		  test_condition(AgentInfo,NewBindings,
				 Conditions,NextCondition)
	  end,
    iterator:create_iterator_fun(ItRes,Fun).


true(AgentInfo,Bindings,Conditions,NextCondition)->

    %% Condition: true
  ItRes = case Bindings of 
	      {?UNBLOCK} ->
		  io:format("[Conditions] Unblock in true function~n"),
		  {?UNBLOCK};

	      {?FAIL} -> 
		  iterator:create_iterator([{?FAIL}]);
	      _ -> 
		  iterator:create_iterator([Bindings]) 
	  end,
    Fun = fun (FinalBindings)-> 
		  test_condition(AgentInfo,FinalBindings,Conditions,
				 NextCondition) end,
    iterator:create_iterator_fun(ItRes,Fun).

false(AgentInfo,_Bindings,Conditions,NextCondition)->
     %% Condition: false
    
    ItRes =  iterator:create_iterator([{?FAIL}]),
    Fun = 
	fun (FinalBindings)-> 
		test_condition(AgentInfo,FinalBindings,
			       Conditions,NextCondition) end,

    iterator:create_iterator_fun(ItRes,Fun).



query_bb(AgentInfo,Bindings, Conditions,Query,NextCondition)->
    %% Condition: search for a belief/rule
    %% Function that generates all potential results (bindings)
    ItRes = case Bindings of 
	      {?UNBLOCK} ->
		  io:format("[Conditions] Unblock in queryBB function~n"),
		    iterator:create_iterator([{?FAIL}]);

		{?FAIL} -> 
		    iterator:create_iterator([{?FAIL}]);
		_ -> 
		    case belief_base:query_bb(AgentInfo,Bindings,Query) of
			false -> 
			    iterator:create_iterator([{?FAIL}]);
			It when is_function(It) -> It end end,

    %% Attempts next condition for each valid valuation
    Fun = fun (NewBindings) -> 
		  test_condition(AgentInfo,NewBindings,
				 Conditions,NextCondition) end,
    iterator:create_iterator_fun(ItRes,Fun).



log_not(AgentInfo,Bindings, Conditions,CheckCondition,NextCondition)->
  %% Condition: log not 
    ItRes = 
	case Bindings of 
	      {?UNBLOCK} ->
		  io:format("[Conditions] Unblock in lognot function~n"),
		  {?UNBLOCK};

	    {?FAIL} ->  
		iterator:create_iterator([{?FAIL}]);
	    _ -> 
		It =  
		    test_condition(AgentInfo,Bindings,
				   Conditions,CheckCondition),
		Cond = fun ({?FAIL}) -> 
			       false;
			   (_) -> 
			       true end,
		case iterator:any(Cond,It) of 
		    %% The condition after the "not" is fulfilled, 
		    %% then ?FAIL is returned
		    true ->    
			iterator:create_iterator([{?FAIL}]);
		    
		    %% The condition after the "not" cannot be fulfilled,
		    %% then Bindings is returned
		    false ->   
			iterator:create_iterator([Bindings])
		end
	end,
    Fun = fun (NewBindings) -> 
		  test_condition(AgentInfo,NewBindings,
				 Conditions,NextCondition)
	  end,
    iterator:create_iterator_fun(ItRes,Fun).


operation(AgentInfo,Bindings,Conditions,{operations,log_not,
			      {Mod,Func,Args}},
	  NextCondition)->
    
%% Condition:  (binary) operation 

  ItRes = case Bindings of 
	      {?UNBLOCK} ->
		  io:format("[Conditions] Unblock in operation function~n"),
		  {?UNBLOCK};

	      {?FAIL} -> iterator:create_iterator([{?FAIL}]);
	      _ -> 
		  %% Execute {Mod,Func,Args} first
		  Result =
		      case {Mod,Func} of
			  {belief_base,query_bb} ->
			      %% It
			      It = apply(Mod,Func,[AgentInfo,Bindings|Args]),
			      case iterator:first(It) of
				  false ->
				      {?FAIL};
				  Other when is_list(Other) ->
				      {replace_bindings,Other}
			      end;
			  {operations,operation}->
			      apply(Mod,Func,[Bindings|Args]);
			  {actions,internal_action} ->
			      apply(Mod,Func,[AgentInfo,Bindings|Args])
		      end,
		  		     
		  
		  %% Result is either {?FAIL} or {replace_bindings,NewBindings}

		  case apply(operations,log_not,[Bindings,Result]) of
		      {?FAIL} -> 
			  iterator:create_iterator([{?FAIL}]);
		      {replace_bindings,ItUpdatedBindings}-> 
			  ItUpdatedBindings
		  end
	  end,
    
    Fun = fun (FinalBindings)-> 
		  test_condition(AgentInfo,FinalBindings,
				 Conditions,NextCondition)
	  end,
    iterator:create_iterator_fun(ItRes,Fun);
operation(AgentInfo,Bindings,Conditions,{operations,operation,Args},
	  NextCondition)->
	  
%% Condition:  (binary) operation 

  ItRes = case Bindings of 
	      {?UNBLOCK} ->
		  io:format("[Conditions] Unblock in operation2 function~n"),
		  {?UNBLOCK};

	      {?FAIL} -> iterator:create_iterator([{?FAIL}]);
	      _ -> 
		  
		  case apply(operations,operation,
			     [Bindings|Args]) of 
		      {?FAIL} -> 
			  iterator:create_iterator([{?FAIL}]);
		      {replace_bindings,ItUpdatedBindings}-> 
			  ItUpdatedBindings
		  end
	  end,
    
    Fun = fun (FinalBindings)->
		  test_condition(AgentInfo,FinalBindings,Conditions,
				  NextCondition)
	  end,
    iterator:create_iterator_fun(ItRes,Fun).




internal_action(AgentInfo,Bindings,Conditions,Package,InternalAction,
		NextCondition)->
    
    %% Condition: internal action

    ItRes = case Bindings of 
	      {?UNBLOCK} ->
		  %% io:format("[Conditions] Unblock in intAct function~n"),
		  {?UNBLOCK};

		{?FAIL} -> iterator:create_iterator([{?FAIL}]);
		_ -> 
		  case 
		      apply(actions,internal_action,[AgentInfo,Bindings,
						     Package,InternalAction]) of
		      
		      {?FAIL} -> 
			  iterator:create_iterator([{?FAIL}]);
		      
		      {replace_bindings,UpdatedBindings} when 
			is_list(UpdatedBindings)-> 
			  iterator:create_iterator([UpdatedBindings]);
		      
		      {replace_bindings,ItBindings} when 
			is_function(ItBindings) ->
			  ItBindings;
		      {?ACTIONSUSPEND,ID,SuspendedAction}->
			  %% ID  is {?SM, TS} or {?DM,TS}
			  case wait_for_response(Bindings,SuspendedAction,
						 ID) of
			      {?FAIL} -> 
				  %% The internal action failed
				  iterator:create_iterator([{?FAIL}]);
			      NewBindings ->
				  %% The internal action suceeded
				  iterator:create_iterator([NewBindings])
			  end
		  end
	    end,

    Fun = fun (FinalBindings)-> 
		  test_condition(AgentInfo,FinalBindings,Conditions,
				 NextCondition)
	  end,
    iterator:create_iterator_fun(ItRes,Fun).


%% This function is invoked when a condition is an internal action whose
%% resolution requires the interaction with either ?DM or ?SM
%% Note that the process becomes "blocked" in the receive action
wait_for_response(Bindings,SuspendedAction,{Manager,TS}) ->
    receive 
	{Manager,{TS,Response}} ->
	    process_response(Bindings,SuspendedAction,Response)
    end.


%% NOTE: the side-effects (e.g. send a message, disconnect a container)
%%       included in plan context or rules are not guaranteed to happen.
%%       The rationale behind this decision is discouraging its use

%% Returns either {?FAIL} or Bindings
process_response(Bindings,#suspended_send{},#send_message_response{
					   result = Result})->
    case Result of
	?NOAGENT ->
	    {?FAIL};
	?AGENTFOUND ->
	    Bindings
    end;
process_response(Bindings,#suspended_connect{}, 
		 #connection_response{result = Result}) ->
    
    case Result of
	?CONNECTED ->
	    Bindings;
	_ ->
	    {?FAIL}
    end;		  
process_response(Bindings, #suspended_disconnect{}, _) ->
    Bindings;
process_response(Bindings,#suspended_create_agent{},
		 #create_agent_response{result = Result}) ->
    case Result of
	?CREATED ->
	    Bindings;
	_ ->
	    {?FAIL}
    end;		  
process_response(Bindings,#suspended_kill_agent{},_) ->
    Bindings;
process_response(Bindings,#suspended_find_agent{},
		 #find_agent_response{result = Result}) ->
    case Result of
	?NOAGENT ->
	    {?FAIL};
	?AGENTFOUND ->
	    Bindings
    end;
process_response(_Bindings,
		 #suspended_find_container{ container_var = ContainerVar,
					    use_bindings = UseBindings},
		 #find_agent_response{result = Result,
				      container= ContainerName})->
    
    case Result of 
	?NOAGENT ->
	    {?FAIL};
	?AGENTFOUND ->
	    %% Agent found in ContainerName, then match ContainerVar
	    ResultVar =
		#var{functor = ContainerName,
		     id = ContainerName,
		     args = ?ISATOM,
		     annots = []},


	    Bindings =
		orddict:store(
		  ContainerName,
		  ResultVar,
		  UseBindings),
    
	    case variables:match_vars(Bindings,ResultVar,ContainerVar) of
		false ->
		    {?FAIL};
		
		ItNewBindings ->
		    case iterator:first(ItNewBindings) of
			false ->
			    {?FAIL};
			FinalBindings ->
			    FinalBindings
		    end
	    end
    end;
process_response(_Bindings,SuspendedAction,Response) ->
     io:format("[Conditions] Wrong response with Params: ~n"++
	      "Response: ~p~n"++
	      "SuspendedAction: ~p~n",
	       [Response,SuspendedAction]),
    ?STOP,
    {?FAIL}.
