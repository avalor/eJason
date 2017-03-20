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
%% @copyright 2012-2013 Álvaro Fernández Díaz
%% @doc Implements the reasoning cycle of an eJason agent

-module(reasoningCycle).

-export([start/5,reasoningCycle/1, applyChanges/2,selectEvent/1,
	selectPlan/1,selectIntention/1]).
%-compile(export_all).


-define(NULL, []).
-include("include/ejason.hrl").
-include("include/macros.hrl").
-include("include/parser.hrl").
-include("include/variables.hrl").
-include("include/dm_responses.hrl").
-include("include/dm_sm_responses.hrl").
-include("include/sm_responses.hrl"). %% It includes the #monitor_notification
-include("include/suspendedActionsRecords.hrl").


% Initializes an agent's mental state
start(AgentName,EnvironmentName,InitialEvents, Plans,BB)->
   %%  case whereis(AgentName) of 
   %% 	undefined ->
   %% 	    %% Register myself
   %% 	    register(AgentName,self());
   %% 	Pid when Pid == self()->
   %% 	    %% TODO: only register an agent at a single point
 
   %% 	    %% The ?DM registered me already
   %% 	    ok;
   %% 	_ ->
   %% 	    exit(name_clash_at_agent_start)
   %% end,



    #agentRationale{agent_name = AgentName,
		    plans = Plans,
		    environment = EnvironmentName,
		    belief_base = BB,
		    events = InitialEvents}.


reasoningCycle(#agentRationale{ events = [],
				retried_events = Retried,
				info = {Iterations,WaitTime,_MChecked},
				intentions = []}= Agent)->

    %% The wait time is doubled every 10 iterations up to a maximum of 1 sec.
    %% {NewIterations,NewWaitTime} = 
    %% 	if 
    %% 	    WaitTime >= 10000 ->
    %% 		{0,WaitTime};
    %% 	    true ->
    %% 		if 
    %% 		    Iterations >= 10 ->
    %% 			{0,WaitTime *2};
    %% 		    true ->
    %% 			{Iterations +1,WaitTime}
    %% 		end
	%% end,
    
   %% case Retried of
   %%     [] ->
   %% 	   ok;
   %%     _ ->
   %% 	   io:format("Retried events: ~p~n",[Retried])
   %% end,
    
	   
       

    %%timer:sleep(NewWaitTime), %% Comment to skip process sleep time
    %io:format("Slept ~p milliseconds~n",[NewWaitTime]),
    NewAgent = check_mailbox_or_wait(Agent),
    reasoningCycle(NewAgent#agentRationale{info={Iterations,WaitTime,
						?MBOXCHECKED}});
reasoningCycle(OldAgent= #agentRationale{info={_NewIterations,_NewWaitTime,
						MailCheck}})->
   %io:format("Initial Agent: ~p~n",[Agent]),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %STEP #1: check mailbox, if not just done
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    Agent = case MailCheck of
		   ?MBOXCHECKED->
		       OldAgent;
		   ?MBOXNOTCHECKED -> 
		       check_mailbox(OldAgent)
	       end,
    
   #agentRationale{selectEvent = EvSel, 
		   events = Events,
		   retried_events = RetriedEvents,
		   agent_name = AgentName,
		   belief_base = BB,
		   module_name = ModuleName,
		   plans = Plans,
		   intentions = Intentions,
		   selectPlan = OptionSelector,
		   %% suspended = SuspendedIntentions,
		   selectIntention = IntentionSelector} =
	Agent,

    %% Information used by the different parts of a plan 
    AgentInfo = #agent_info{
      belief_base = BB,
      agent_name = AgentName,
      module_name = ModuleName},
     

   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
   %STEP #2: choose one event using the event selection function
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
   {Event, NotChosenEvents} = EvSel(Events),
     %io:format("The set of events is: ~p~n The suspended events are: ~p~n",
     %	      [Events, RetriedEvents]),
    
    EventPrinter = fun (SomeEvent) ->
			   if is_record(SomeEvent,event)-> 
			      io_lib:format("Event: ~p",
					    [{SomeEvent#event.type, 
					      SomeEvent#event.body}]);
			      true ->
				   ""
			   end
		   end,
    
    %% if is_record(Event,event) andalso
    %%    AgentName == testVarsMatching ->
    	    %% io:format("Chosen ~s~n",[EventPrinter(Event)]),
    	    %% io:format("NOT CHOSEN EVENTS:~s~n",[
    	    %% 					lists:map(EventPrinter,
    	    %% 						  NotChosenEvents)]),
    %%    true ->
    %%  	    ok
    %%  end,
    
	    
    


    %%  case AgentName of
    %%     owner ->
    %%	  io:format("~p BB: ~p~n",[AgentName,BB]);
    %%    io:format("~p Selected Event: ~p~n",[AgentName,Event]),
    %%     _ -> 
    %%   {}
    %% end,
    
      
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
   %STEP #3: find intended means for the chosen event. 
   %
   % The intended means are a plan record where the return params and
   % the variables in the trigger and the context may be already bound
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    IntendedMeans = 
	findIntendedMeans(AgentInfo,Event,Plans,OptionSelector),



   %% case RetriedEvents of
   %%     [] ->
   %% 	   ok;
   %%     _ ->
   %% 	   io:format("\n\nRetried events in normal cycle: ~p~n",[RetriedEvents]),
   %% 	   io:format("NewEvent: ~p~n",[NewEvent])
	   	   
   %% end,
    


    %% Adding event to retried_events (events that will be retried)
    %% when IntendedMeans is [] 
    %% Events retried only when some +belief or -belief occurs or when
    %% a new plan is added to the plan base
    NewRetriedEvents =
	case {Event,IntendedMeans} of
	    {#event{type = ?ADDACHGOAL}, []} ->
		lists:reverse([Event|lists:reverse(RetriedEvents)]);
	    {#event{type = ?ADDWAITTESTGOAL}, []} ->
		lists:reverse([Event|lists:reverse(RetriedEvents)]);
	    _ ->
		RetriedEvents
	end,


    %% case NewRetriedEvents of
    %% 	[] ->
    %% 	    io:format("New Retried Events: []~n");
    %% 	_ ->
    %% 	    io:format("New Retried events:~n"),       
    %% 	    lists:map(fun (SomeEvent)->
    %% 			      io:format("~s~n",
    %% 					[EventPrinter(SomeEvent)])
    %% 		      end,
    %% 		      NewRetriedEvents)
    %% end,

    %% io:format("EVENT:~p~nIntendedMenas~p~n",
    %% 	      [Event, IntendedMeans]),

    %% Add a new failure event if a test goal/failed test goal cannot be matched
    NewEventSet =
	case {Event, IntendedMeans} of
	    {#event{type = ?ADDTESTGOAL}, []} ->
		TestGoalFailureEvent = make_failure_event(Event),
		[TestGoalFailureEvent|NotChosenEvents];
	    {#event{type = ?FAILEDTESTGOAL}, []} ->
		PlanFailureEvent = make_failure_event(Event),
		[PlanFailureEvent|NotChosenEvents];
	    _  ->
		NotChosenEvents
	end,
    
    %% Agent used after intended means identification
    IMAgent =
	Agent#agentRationale{
	  events = NewEventSet,
	  retried_events = NewRetriedEvents},
       
    
    
	    
    %% if AgentName == silly ->
    %% 	    %% io:format("[RC ~p] INTENDED MEANS: ~p~n",
    %% 	    %% 	      [AgentName, IntendedMeans]),
    %% 	    case IntendedMeans of
    %% 		[] ->
    %% 		    "[RC] IntendedMeans: []";
    %% 		_ when is_record(IntendedMeans,plan)->
    %% 		    io:format("[RC ~p] INTENDED body: ~p~n",
    %% 			      [AgentName,
    %% 			       IntendedMeans#plan.body]);
    %% 		_ ->
		    
    %% 		    io:format("[RC ~p] INTENDED body: ~p~n",
    %% 			      [AgentName,
    %% 			       IntendedMeans])

    %% io:format("INTENDED bindings: ~p~n",[IntendedMeans#plan.bindings]),
    %% io:format("INTENDED return: ~p~n",[IntendedMeans#plan.return_variables]),
    %% 		end;
    %%    true -> ok
    %% end,
	    
    %%   io:format("NewEvent: ~p~n",[NewEvent]),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %STEP #4: process intended means (create new/update intention stack..)
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    AllIntentions = 
	processIntendedMeans(Event,Intentions,IntendedMeans),
     %%io:format("All Intentions: ~p~n",[AllIntentions]),
    


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %STEP #5: choose one intention to execute. Special cases for agent
    %         termination are considered.
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    case IntentionSelector(AllIntentions) of
	{{terminate,kill},_}->
	    ok;
	%% io:format("Agent ~p killed.~n",
	%%[IMAgent#agentRationale.agent_name]);
	{{terminate,kill,TestPid},_} when is_pid(TestPid)->
	    TestPid ! {killed, #agentRationale.agent_name};
	    %io:format("Agent ~p killed. Answer sent to ~p~n",
	%	      [Agent#agentRationale.agentName,TestPid]);
	[] ->
	   % reasoningCycle(Agent);
	    ?MODULE:reasoningCycle(IMAgent#agentRationale{
				     info={0,1,?MBOXNOTCHECKED}});
	
	{Intention,NotChosenIntentions} ->
	    %io:format("Chosen Intention: ~p~n",[Intention]),
	    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %STEP #6: execute the intention.
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	    UseAgent  =
		IMAgent#agentRationale
		  {intentions = NotChosenIntentions},
    
	    %% New agent may contain new events/beliefs
	    %% as result of the execution
	    %% of a critical section
	   {NewAgent,Changes} = executeIntention(UseAgent,Intention),
	   %io:format("Result from Execute intention is ~p~n",[Changes]),


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %STEP #7: apply the proper changes to the mental state with 
    %         respect to the last iteration
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	    FinalAgent = 
		applyChanges(NewAgent,
			     Changes),
%	   io:format("FinalAgent: ~p~n",[FinalAgent]),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %STEP #8: iterate the reasoning cycle with the new mental state.
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


	   ?MODULE:
		reasoningCycle(
		  FinalAgent#agentRationale{
		    info={0,1,?MBOXNOTCHECKED}});%% Info is RESTARTED
	Other->
	    io:format("[RC for ~p] error: wrong intention chosen:\n ~p~n",
		      [AgentName,Other])
    end.



%% Generates an iterator that can compute all applicable plans
findApplicablePlans(AgentInfo,
		    #event{type = Type,body=EBody},PlanList)->

    %%io:format("Executing find applicable plans~n"),
    %% io:format("PlanList: ~p~n",[PlanList]),
    %%	  io:format("BB: ~p~nType: ~p~nBody: ~p~nPlan: ~p~n",
    %%   [BB,Type,EBody,PlanList]),

    %% io:format("[RC] FindAppPlans: EVENTBODY: ~p~n",[EBody]),


    %% The variables in the body of the event must be replaced
    %% The replacements are stored in "replacements"
    %% The bindings of the input params are stored in "input_vars"
    %% it is done this way to avoid calling variables:update when the plan
    %% is not even executed (because the arity is not the proper one)

    Replacements =
	variables:obtain_replacements("ANONYMVAR"++
				      variables:make_timestamp_string()++
				      "_",
				      1,[EBody]),


      %% io:format("Body Replacements: ~p~n",[Replacements]),
    NewEventBody =
	variables:use_replacements(EBody, Replacements),

    EventBodyVars =
	variables:vars_to_import(NewEventBody), 


    %% io:format("Plan Type: ~p~n",[Type]),
    %% io:format("Plan Body: ~p~n",[NewEventBody]),
    %% io:format("EventBodyVars: ~p~n",[EventBodyVars]),
    
   UseType =
	case Type of
	    ?ADDINTENTIONGOAL ->
		%% Add new intention must be changed here
		?ADDACHGOAL;
	    ?ADDWAITTESTGOAL ->
		%% Add wait test goal must be changed here
		?ADDTESTGOAL;

	    _ ->
		Type
	end,

    ItPlans = 
	case orddict:find(UseType,PlanList) of
	    {ok,TypePlans} ->
		%% io:format("Plans for that type: ~p~n",
		%% 	  [TypePlans]),
		%% Plans with the same type as the event
		iterator:create_iterator(TypePlans);
	    error ->
		%% io:format("No plans of that type~n"),
		iterator:create_iterator([])
	end,
    



    MatchFun =  
	fun (Plan) -> 
		NewPlan = 
		    Plan#plan{
		      input_vars = EventBodyVars,
		      replacements = Replacements
		     },
		%% io:format("Trigger: ~p~n",[Plan#plan.trigger]),
		match_plan(AgentInfo,UseType,
			   NewEventBody,
			   NewPlan) end,
    iterator:create_iterator_fun(ItPlans,MatchFun).


%% Recall that the trigger of a plan contains:
%%  1) The variable to match (trigger)
%%  2) The first formula to compute the context

%% The context is then a series of conditions represented by an
%% orddict
match_plan(AgentInfo,EventType,EventBody,
	   Plan = #plan{trigger={TriggerVar,
				 FirstContextFun},
			context = Context,
			bindings=Bindings,
			input_vars = InputVars}) ->

 
    %% EventBody is a variable that represents the struct for the trigger

    %% TODO: try to delay this call to not be called if the plan arity is diff.
    %% The variables in the input vars are added to the bindings of the plan
    %% No possible clash should exist for non-atomic vars

    AllBindings = 
	variables:update(Bindings,InputVars),



    Params  = [AgentInfo,AllBindings,Context,TriggerVar,
	       variables:get_var(EventBody#var.id,
				 AllBindings), FirstContextFun],
    
		
    %% io:format("[RC] Checking trigger + context~n"),
    %% timer:sleep(2000),
    case apply(conditions,trigger,Params) of
	false -> 
	     %% io:format("[RC] Trigger/Context not matched (Plan not Applicable)~n"),
    	    false; % The plan is not applicable
    
	ItBindings when is_function(ItBindings) -> % The trigger matches
    	     %% io:format("[RC]Matched Trigger\n"),
	    	    
    	    PlanFun = 
    		fun (NewBindings) when is_list(NewBindings)->
			%% io:format("[RC]Bindings for Plan: ~p\n", [NewBindings]),
			
	       		%% The plan is executed using NewBindings
    			Plan#plan{bindings = NewBindings};
    		    %% (false) ->
    		    %% 	%% io:format("The context cannot be matched~n"),
    		    %% 	false;
		    (A) ->
    			io:format("[RC]error: match for"++
    				  " context returned: ~p~n",[A]),
			io:format("For plan: ~p\nwith params: ~p~n\n",
				  [TriggerVar,Params]),
    			exit(reasoning_cycle_error)
    		end,
    	    iterator:create_iterator_fun(ItBindings,PlanFun);
 

   	Other ->
    	    io:format("[RC]error match for conditions:trigger returned: ~p~n",
		      [Other])
    end.
%% true ->
	%%     %% This plan is not applicable because the arity is different
	%%     %%io:format("Error: badarity~n",[]),
	%%     false

%%  catch
%% 	error:{badarity,_Error}-> %% The plan is not applicable
%% %	    io:format("Error: badarity~n",[]),
%% 	    false% ;
%% %	  A:B ->
%% %	    io:format("[RC] Unexpected error: ~p:~p~n \n\n",[A,B]),
%% %	    exit(avalor)
    


    
	

%%
%% Intention = [{Event,Plan,Valuation,[{FormulaFunction,Bindings}] }
%% Last event and plan are included to enhance observational power of
%% the intention selection function.
%%
%% The plan also includes the replacements done to the eventbody
%%
%% Generates a new intention ([#piplan]) from the intended means
%% or updates an existing one
%%
processIntendedMeans(_Event,Intentions,[]) ->
    Intentions;
%% Instead of a plan, the intended means is a new intention, computed
%% after matching a retried wait_test_goal
processIntendedMeans(_Event,Intentions,{?ADDINTENTION,
 					NewIntention}) ->
     [NewIntention|Intentions];
processIntendedMeans(_Event,Intentions,{terminate,kill}) ->
    I = [{terminate,kill}|lists:reverse(Intentions)], 
    %% Terminate only when there are no more intentions
    lists:reverse(I);
processIntendedMeans(_Event,Intentions,{terminate,kill,TestPid}) ->
    I = [{terminate,kill,TestPid}|lists:reverse(Intentions)], 
    %% Terminate only when there are no more intentions
    lists:reverse(I);
processIntendedMeans(Event,Intentions,
		     #plan{body = FormulaList,
			   bindings = Bindings}=Plan) ->
    
 %    io:format("Event: ~p~n",[Event]),
 %   io:format("Bindings so far: ~p~n",[Bindings]),

				     %%Remove initial bindings
%    io:format("Removing bindings: ~p~nRetFormulas: ~p~n",
%	      [Bindings,Plan#plan.return_params]),

    % Param bindings necessary to generate the plan failure event
  %  io:format("RetParams: ~p~n",[RetParams]),

    %%InitParams = RetParams, 
	%variables:valuate_params(RetParams, Bindings),

 %   io:format("RetAnnots: ~p~n",[RetAnnots]),
    %%InitAnnots = RetAnnots,%variables:valuate_annots(RetAnnots, Bindings),
 %  io:format("InitAnnots: ~p~n",[InitAnnots]),

    %%InitBindings = {InitParams,InitAnnots},
    
   % io:format("InitBindings: ~p~n",[InitBindings]),
    NewIntention =  
	case Event#event.relatedIntention of
	    []->
		%% A new intention is generated
		[#piPlan{ event = Event,
			  %% done to save space
			  plan = Plan#plan{bindings = []},
			  valuation = Bindings,
			  init_bindings = Bindings,
			  formulas = FormulaList}];
	    RIntention->
		[#piPlan{id = IntentionID}|_] =
		    RIntention,
		
		NewSIPlan = #piPlan{ event = 
				     Event#event{relatedIntention = []},
				     %% done to save space
				     plan = Plan#plan{bindings = []},
				     valuation = Bindings,
				     init_bindings = Bindings,
				     id = IntentionID,
				     formulas = FormulaList},
		[NewSIPlan|RIntention]
	end,
    [NewIntention|Intentions].


%% Executes the intention chosen: i.e. the formula on top of it
%% Returns {NewAgent,Changes}  where:
%%    Changes is a list of state updates (e.g. adding new events 
%% or updating intention list)

%% executeIntention(Agent,[])-> 
%%     io:format("No intention executable"),
%%     {Agent,[]};
executeIntention(Agent,Intention = [PIPlan|_RestPIP])->
%    io:format("Executing Intention: ~p~n",[Intention]),
    Valuation = PIPlan#piPlan.valuation,
    %io:format("Agent is: ~p~n",[Agent]),

    AgentInfo = #agent_info{
      belief_base = Agent#agentRationale.belief_base,
      agent_name = Agent#agentRationale.agent_name,
      module_name = Agent#agentRationale.module_name},
    

%%    io:format("Valuation: ~p~n",[Valuation]),
    case PIPlan#piPlan.formulas of
	[{critical_section, Formulas}|_]->

	    CriticalID =erlang:timestamp(),
		
	    %% A "new intention" is created with the formulas in the 
	    %% critical section. This section is not left until all
	    %% the formulas have been executed or when a failure arises.
	    {NewAgent,Result}= execute_critical_section(
			Agent,[PIPlan#piPlan{
				 id =CriticalID,
				 formulas=
				 Formulas++
				 [{end_critical,CriticalID}]}]),
	    %% NewAgent may differ from Agent in the number of events/beliefs
	    {NewAgent,processAnswer(Intention,Result)};
	[{Module,Fun,Args}|_] ->
	    
	     %% io:format("[RC ~p] Executing formula: ~p ~p~n",
	     %% 	      [AgentInfo#agent_info.agent_name,Module,Fun] ),
	     %% io:format("[RC ~p] Args: ~p~nValuation: ~p~n",
	     %% 	      [AgentInfo#agent_info.agent_name,Args,Valuation] ),
	    
	    
	     %% io:format("AgentInfo given: ~p~n",[AgentInfo]),
	    Result = 
		try
		    case Module of
			%%Body Formulas are either:
			actions -> 
			    %% Internal/External actions 
			    apply(Module,Fun,[AgentInfo,Valuation|Args]);
			operations ->
			    %% Arithmetic/Logical operationcs
			    apply(Module,Fun,[Valuation|Args]);
			utils ->
			    %% Add/Remove goals/beliefs 
			    apply(Module,Fun,[Valuation|Args]);
			Other ->
			    io:format("[RC DEBUG: wrong body formula: ~p~n",
				      [{Module,Fun,Args}]),
			    timer:sleep(5000),
			    exit(error)
		    end
		catch
		    _:_->
			%% io:format("[~p] ExecuteIntentionFailed for: ~p~n",
			%% 	  [?MODULE,Intention]),
			{?FAIL}
		end,
	    						   
		    
			 
	     %% io:format("Result of execution: ~p~n",[Result]),
	    {Agent,processAnswer(Intention, Result)};

	[] ->
%	    io:format("No more Formulas\n"),
	    case furtherInstantiatePlan(Intention) of
		[{addIntention,NewIntention}] ->
	      %% 	    io:format("Finally adding Intention: ~p~n", 
	      %% [NewIntention]),
		    executeIntention(Agent,
				     NewIntention);
		[{deleteIntention,[]}] ->
		    {Agent,[{deleteIntention,[]}]}
	    end
    end.
%    io:format("AnswerList: ~p~n",[Result]),
 %   processAnswer(ModuleName,Intention, Result).


%% Executes, uninterruptedly, all the formulas in a critical section
%% The suspension of intentions in a critical section, suspends the activity 
%% of the agent (except checking for the reception of messages that reactivate
%% the suspended intentions).
execute_critical_section(Agent, 
			 [#piPlan{id = CriticalID,
				  formulas = [{end_critical,CriticalID}],
				  valuation = NewValuation}])->
    %% The critical section has been executed completely
    {Agent, {replace_bindings, NewValuation}};

%%TODO: exit the critical section if a plan failure is not "trapped"

execute_critical_section(Agent, Intention = [#piPlan{id = CriticalID}|_])->
 %% io:format("Wait\n"    
    %% 1) Execute a formula in the critical section
    {NewAgent,Changes} = executeIntention(Agent,Intention),


    %% 2) Apply the proper changes
    IntermediateAgent =
	applyChanges(NewAgent, Changes),

    %% 3) Process the possible messages from ?DM and ?SM

    FinalAgent =
	check_mailbox_critical_section(IntermediateAgent),
    
    {[CriticalIntention],OtherIntentions} =
	%% Extract the critical intention
	lists:partition(
	  fun([#piPlan{id = ID}|_]) when ID == CriticalID->
		  true;
	     (_) ->
		  false
	  end,
	  FinalAgent#agentRationale.intentions),

   %% io:format("CriticalIntention: ~p~n",[CriticalIntention]),

    %% 4) Iterate over the same intention
    execute_critical_section(
      FinalAgent#agentRationale{intentions = OtherIntentions}, 
      CriticalIntention).%% ;
%% execute_critical_section(A,B) ->
%%     %% io:format("A: ~p~n",[A]),

%%     io:format("B: ~p~n",[B]),
%%     exit(error).

    
	
				      
			 
    


%% Updates the valuation of the PIP with the new bindings in the 
%% obtained valuations 
%% Removes the last executed formula from the body of the PIP 
%% and removes the PIP after the execution of the last formula.
%%
%% Returns either [{deleteIntention, []}] or [{addIntention,_}]
furtherInstantiatePlan([])->  
    [{deleteIntention,[]}];
furtherInstantiatePlan(
  [PIP = #piPlan{formulas = Formulas,
		 valuation = Bindings,
		 plan=#plan{
		   replacements = Replacements}
		}|RestPIP])->

    %io:format("Instantiate with return_params: ~p~n",[Replacements]),
    %%  {[NewValuation],RestFormulas} = 
    %%  io:format("Formulas: ~p~n",[Formulas]),
    NewIntention = 
	case Formulas of
	    [] ->
		%% io:format("RestSip: ~p~n",[RestSIP]),
		case RestPIP of 
		    [] -> %% No more plans in the intention
			[{deleteIntention,[]}];

		    %% There are more piplans in the intention
		    [NextPIP = #piPlan{formulas = _NextFormulas,
				       valuation = OldBindings}|Rest] ->
			%% io:format("The oldBindings are: ~p~n",
			%% 	  [OldBindings]),
			%% io:format("The old replacements are: ~p~n",
			%% 	  [Replacements]),

			NewBindings =
			    variables:import_new_matchings(
			      OldBindings, Replacements,
			      "VARFROMPLANCALL"++
			      variables:make_timestamp_string()++
			      "_",
			      Bindings),

			%% io:format("NewParams: ~p~n",[NewParams]),
			%% io:format("NewValuation: ~p~n",[NewValuation]),
			case furtherInstantiatePlan(
			       [NextPIP#piPlan{valuation = NewBindings}|
				Rest]) of
			    [{addIntention,NI}] ->
				NI;
			    [{deleteIntention,[]}] ->
				[{deleteIntention,[]}]
			end
		end;
	    
	    [_ExecutedFormula|NextFormulas] ->
		%% Remove the formula on top 
		[PIP#piPlan{formulas = NextFormulas}|RestPIP]
	end,

    %% io:format("NewIntention: ~p~n",[NewIntention]),

    case NewIntention of 
	[{deleteIntention,[]}] ->
       %% io:format("NewIntention at last: ~p~n",[NewIntention]),
	    [{deleteIntention,[]}];
	_ ->    
	    [{addIntention,NewIntention}]
    end.
		       



%% Process the answer obtained during the last intention execution
%% Returns a list of the changes for the mental state of the agent
processAnswer(Intention,Answer)->
     %% io:format("Processing ANSWER: ~p~n",[Answer]),
    
    Result = case Answer of
		 %% {finished, NewValuation}->
		 %%     SIP completely executed
		 %%     furtherInstantiatePlan(Intention,NewValuation);
		
		 #event{type = EventType} when EventType == ?ADDTESTGOAL->
		     %%io:format("Finished test goal ~n"),
		     %% test goal events are generated when a test
		     %% goal cannot be matched straightaway (instead
		     %% of generating a plan failure).
		     [Answer#event{relatedIntention=Intention}];%

		 #event{type = EventType} when EventType == ?ADDWAITTESTGOAL->
		     %% wait test goal events are generated when a
		     %% wait test goal cannot be matched
		     %% straightaway. They will be retried.
		     [Answer#event{relatedIntention=Intention}];%


		 #event{type = ?ADDACHGOAL,
			corrected_bindings = CorrectedBindings}->
		     [CurrentPIPlan|Rest] = Intention,

		     %% Bindings corrected after invoking the new plan,
		     %% also added [source(self)]

		     NewPIPlan =
			 CurrentPIPlan#piPlan{valuation =CorrectedBindings},

		     NewIntention =
			 [NewPIPlan|Rest],
					      
		     [Answer#event{corrected_bindings =[],
				   relatedIntention=NewIntention}];%++

		 %%furtherInstantiatePlan(Intention,[]);
		 %% {add_ejason_private_query, {QueryName,Args,Annot}}->
		    
		 %%     [#event{type = add_ejason_private_query,
		 %% 	     body ={ QueryName,
		 %% 		     add_ejason_private_query,
		 %% 		     Args,Annot},
		 %% 	     relatedIntention=Intention}];
		 
		 {?STUTTERACTION} ->
		 	 furtherInstantiatePlan(Intention);

		 {suspend,DelegatePID,TimeStamp}->
		     [{suspend,Intention,{DelegatePID,TimeStamp}}];

		 {?ACTIONSUSPEND,ID,SuspendedAction}-> 
		     %% Action is suspended waiting for a message from
		     %%  ?DM or ?SM
		     [{?ACTIONSUSPEND,ID,SuspendedAction,Intention}];

		 {replace_bindings, NewBindings} ->
		     [CurrentPIPlan|Rest] = Intention,

		     if 
			 is_list(NewBindings) ->
			     %%TODO: ALWAYS an iterator
			     %% NewBindings is a valuation
			     NewIntention = 
				 [CurrentPIPlan#piPlan{valuation =
						       NewBindings}|Rest],
			     furtherInstantiatePlan(NewIntention);

			 is_function(NewBindings) ->
			     %% Only the first match is considered in the
			     %% body of a plan
			     
			     case iterator:first(NewBindings ) of
				 false ->
				     processAnswer(Intention,{?FAIL});
				 Valuation->
				     NewIntention = 
					 [CurrentPIPlan#piPlan{valuation =
							       Valuation}|Rest],
				     furtherInstantiatePlan(NewIntention)
			     end
		     end;
		 
				 

		 %% {update_bindings,NewBindings} ->
		 %% %% Typically from a binary operation 
		 %% [CurrentPIPlan|Rest] = Intention,
		 %% Valuation = CurrentPIPlan#piPlan.valuation,
		 %% NewValuation = variables:update(Valuation, 
		 %% 				     NewBindings),
		 %% NewIntention = [CurrentPIPlan#piPlan{
		 %% 		       valuation =
		 %% 		       NewValuation}|
		 %% 		     Rest],
		 %% furtherInstantiatePlan(NewIntention);
		 
		 #event{type = ?ADDBELIEF,body = Belief}->
		     case variables:is_ground(Belief) of
			 true ->
			     %% io:format("~p IS GROUND~n",[Belief]),
			     %% The belief to add is ground
			     [Answer|furtherInstantiatePlan(Intention)];
			 false->
			     %% Same as if {?FAIL} had been returned
			     [#piPlan{event = Event}|Rest] = Intention,
			     FEvent = make_failure_event(
					Event#event{
					  relatedIntention = Rest}),
			     [FEvent]
		     end;
		 #event{type = ?REMOVEBELIEF}->
		     [Answer|furtherInstantiatePlan(Intention)];
		 #event{type = ?REMOVEADDBELIEF,body = Belief}->
		     case variables:is_ground(Belief) of
			 true ->
			     
			     %% The belief to add is ground
			     [Answer|furtherInstantiatePlan(Intention)];
			 false->
			     %% Same as if {?FAIL} had been returned
			     [#piPlan{event = Event}|Rest] = Intention,
			     FEvent = make_failure_event(
					Event#event{
					  relatedIntention = Rest}),
			     [FEvent]
		     end;
		 #event{type = ?ADDINTENTIONGOAL}->
		     %% io:format("Add New Intention: ~p~n",
		     %% 	       [Answer#event.relatedIntention]),
		     [Answer|furtherInstantiatePlan(Intention)];

		 #event{type = ?FAILEDTESTGOAL, body = FailedQuery}->
		     FEvent = Answer#event{
				relatedIntention = Intention},
		     [FEvent]; 
		 {?FAIL}->
		     [#piPlan{event = Event}|Rest] = Intention,
		     FEvent = make_failure_event(Event#event{
						   relatedIntention = Rest}),
		     
		      %% io:format("[RC] FailureEvent: ~p~n",
		      %% 	       [FEvent]),
		     [FEvent];  			 
		     		      
		 _ ->
		     io:format("[RC, exit] Unknown Answer: ~p~n",[Answer]),
		     exit(unkown_anwer_fatal_error),
		     []
	     end,
    Result.


%% Applies the proper changes to the mental state of the agent.
%% This includes adding/removing beliefs or goals, as well as 
%% generating the proper events
applyChanges(Agent,[]) ->
%   io:format("Final Agent: ~p~n",[Agent]),
   Agent;
applyChanges( #agentRationale{events = Events,
			      retried_events =RetriedEvents,	      
			     intentions = Intentions,
			     belief_base = BB,
			     suspended = Suspended}=Agent,
	     [Change |Changes])->


    %% case RetriedEvents of 
    %% 	[]->
    %% 	    ok;
    %% 	_ ->
    %% 	    io:format("Retried events when in applychanges: ~n~p~n",
    %% 		      [RetriedEvents])
    %% end,

     %%io:format("Change: ~p~n",[Change]),
     %% io:format("Is event: ~p~n",[is_record(Change,event)]),

    NewAgent = case Change of
		   {deleteIntention,_}->
		       Agent;
		   {addIntention,NewIntention} ->

		       %%   io:format("Add: ~p~n", [NewIntention]),
		       Agent#agentRationale{intentions = [NewIntention|Intentions]};	   
		   %% #event{type = add_ejason_private_query} ->
		   %% 	Agent#agentRationale{ events = 
		   %% 			      [Change|Events]};


		   #event{type = ?ADDBELIEF, body = Body} ->
		       %%     io:format("Body: ~p~n",[Body]),
		       Belief = Body,%%utils:vars_to_bindings(Body),
		         %io:format(" [RC] Adding Belief: ~p~n",
			 %	  [Belief]),
		       case belief_base:add(Belief,BB) of
			   {ok,no_change}->
			       %% io:format("NO CHANGE~n"),
			       Agent; %% No change
			   {ok,NewBB, AddedBelief}->
			  %        io:format("NewBB: ~p~n",[NewBB]),
			       %%       {Name,Args,Annot} = Belief,
			       NewEvent =  #event{type=?ADDBELIEF, 
						  body = AddedBelief},

%%% New belief events  have appeared since the last attempt to match the
%%% retried events. Therefore, they are added to the list of events 
			       %io:format("[RC]Retrying evens: ~p~n",[RetriedEvents]),


			       NewEventSet =
				   [NewEvent|Events]++RetriedEvents,
			       

			       %% io:format("Added belief. New Event: ~p~n",
			       %% 		 [NewEvent]),
			       Agent#agentRationale{belief_base = NewBB,
						    events = NewEventSet,
						    retried_events = []}
		       end;


		   #event{type = ?REMOVEBELIEF, body = Body} ->
		       Belief = Body,
		       %% io:format(" [RC] Removing Belief: ~p~n",
		       %% 		 [Belief]),
		     
		       case belief_base:remove(Belief,BB) of
			   {ok, no_change}->
			       Agent;
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
		
%%% New belief events have appeared since the last attempt to match the
%%% retried events. Therefore, they are added to the list of events 
			      
			       %io:format("[RC]Retrying events: ~p~n",[RetriedEvents]),
			       NewEventSet =
				   lists:append([NewEvents,Events,
						 RetriedEvents]),
    
			       Agent#agentRationale{belief_base = NewBB,
						    retried_events =[],
						    events = NewEventSet}
		       end;

		   #event{type = ?REMOVEADDBELIEF, body = Body} ->
		              %% io:format("Change: ~p~n",[Change]),

		       Belief = Body, %%utils:vars_to_bindings(Body),

		      %% timer:sleep(3000),

						% io:format("Belief: ~p~nBB: ~p~n",[Belief,BB]),

		       case belief_base:remove_add(Belief,BB) of
			   
			   {ok, FinalBB, NewEvents} ->

%%% New belief events have appeared since the last attempt to match the
%%% retried events. Therefore, they are added to the list of events 
			       %io:format("[RC]Retrying evens: ~p~n",[RetriedEvents]),

			       NewEventSet =
				   lists:append([NewEvents,Events,
						 RetriedEvents]),
			       
			       Agent#agentRationale{belief_base = FinalBB,
						    events = NewEventSet,
						    retried_events =[]}
		       end;
		   
		       

		   %% Turn the type into a simple achievement goal
		   #event{type = ?ADDINTENTIONGOAL} ->
		       Agent#agentRationale{
			 events = 
			     [Change#event{type = ?ADDACHGOAL}|Events]};

		   #event{}->
		       %% io:format("AddACHGoal: ~p~n",[Change]),
		       %%#event{type = ?ADDACHGOAL}->       
		       Agent#agentRationale{events = [Change|Events]};

		   ?NOEVENT -> %% This may happen when no failure event can be
		               %% generated after calling an external action
		       %% Also, some such events are used as placeholders
		       Agent;
		   
		   {suspend,Intention, ID}->
		       Agent#agentRationale{
			 suspended= [{ID,Intention}|Suspended]};
		   
		   {?ACTIONSUSPEND, ID,SuspendedAction, Intention}->
		       Agent#agentRationale{
			 suspended= 
			 orddict:store(ID,{SuspendedAction,Intention},
				       Suspended)};
		   Other ->
		       io:format("[RC] Wrong change: ~p~n",[Other]),
		       Agent  % Change ignored
	
	       end,
%    io:format("Next Changes: ~p~n",[Changes]),
    applyChanges(NewAgent,Changes);
applyChanges(Agent,Changes) ->
    io:format("[ReasoningCycle:applyChanges,error]: \nInvalid Agent: ~p or"++
	      "~nChanges list: ~p~n",[Agent,Changes]),
    exit(error).
					 
			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Default selection functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Default event selection function.
%% It returns the latest event
%% TODO: why not randomly? Pros: now notifications are processed in-order
selectEvent([])->
    {?NULL,?NULL};
selectEvent([Event|Events]) ->
    {Event,Events}.
 


%% Default option selection function.  It returns the first applicable
%% plan computed or [] if there are none
selectPlan([])->
    [];
selectPlan(ItPlans) ->
   %% io:format("Selecting a plan~n"),
    case iterator:first(ItPlans) of
	false ->
	    [];
	Plan -> 
	    %%io:format("PLAN SELECTED: ~p~n",[Plan]),
	    Plan
    end.
	    
%% Default intention selection function.
%% It returns one of the intentions randomly
selectIntention([])->
    [];
selectIntention({terminate,kill})->
    {terminate,kill};
selectIntention([{terminate,kill}|Intentions]) when length(Intentions) > 0->
    I = [{terminate,kill}|lists:reverse(Intentions)],
    selectIntention(lists:reverse(I));
selectIntention([{terminate,kill,Pid}|Intentions]) when length(Intentions) > 0->
    I = [{terminate,kill,Pid}|lists:reverse(Intentions)],
    selectIntention(lists:reverse(I));
selectIntention(Intentions) ->
    Length = length(Intentions),
    %% Generates random seed
    {A1,A2,A3} = now(),
    random:seed(A1,A2,A3),
    Random = random:uniform(Length),
    Header = lists:sublist(Intentions,Random-1),
    Rest = lists:sublist(Intentions,Random,Length),
    [Intention|Tail] = Rest,
    RestIntentions = lists:merge([Header,Tail]),
    {Intention,RestIntentions}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Agent Mailbox Handling
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_mailbox(Agent=#agentRationale{})->
    Message = gatherOneMessage(),
    NewAgent = processMessage(Agent,Message),
    %NewAgent = Agent#agentRationale{events = Ev++NewEvents},
    %io:format("NewAgent: ~p~n",[NewAgent]), 
    NewAgent.


%% Only invoked when there are no intentions and no events
%% Then, the agent will wait on a "receive"
check_mailbox_or_wait(Agent=#agentRationale{})->
    NewAgent =
	receive 
	    {signal, Signal} -> 
		%% First process signals (ping, unblock)
		processSignal(Signal),
		Agent;
	    Message->
		processMessage(Agent,Message)
	end,
    %NewAgent = Agent#agentRationale{events = Ev++NewEvents},
    %io:format("NewAgent: ~p~n",[NewAgent]), 
    NewAgent.

%% Only processes messages from ?DM and ?SM
check_mailbox_critical_section(Agent= #agentRationale{}) ->
    receive 
	{signal, Signal} -> 
	    %% First process signals (ping, unblock)
	    processSignal(Signal),
	    Agent
    after 0 ->
	    receive
		Message = {?DM,_} ->
		    processMessage(Agent,Message);
		Message = {?SM,_} ->
		    processMessage(Agent,Message)
	    after
		0 ->
		    Agent
	    end
    end.

	


% Only one message is processed in each iteration.
% Specified in Jason Book

gatherOneMessage() -> 
    receive 
	{signal, Signal} -> 
	    %% First process signals (ping, unblock)
	    processSignal(Signal),
	    gatherOneMessage()
    
    after 0 ->
	    receive Message  ->
		    Message
	    after 
		0->
		    [] 
	    end
    end.


processSignal({ping, Pid}) ->
    Pid ! pong;  %% A ping policy is in place
processSignal(?UNBLOCK) ->
    ok.%% Process already unblocked


%processMessage(Agent,BB,List)->
%    processMessages(Agent,BB,List,[]).



%processMessages(_Agent,_BB,[],Acc)->
						%    lists:reverse(Acc);

%% Returns the modified agent after processing the message
processMessage(Agent = #agentRationale{events = Events,
				       agent_name = AName,
				       suspended = Suspended},Message)->
    %% case Message of
    %% 	[]->
    %% 	    ok;
    %% 	_ ->
    %% io:format("\n[~p ~p] at ~p receives Message:\n  ~p~n",[AName, self(),
    %%  				         erlang:timestamp(),Message]),
    %% end,

    NewAgent = 
	case Message of

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EMPTY MAILBOX
	    [] ->
		Agent;
	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RESPONSES FROM DM

	    {?DM, {ID, Response}}-> 
		
		%% io:format("~p receives Response~p~n",
		%% 	  [Agent#agentRationale.agent_name,
		%% 	   Response]),
		case  orddict:find({?DM,ID}, Suspended) of
		    {ok, SuspendedInfo} ->
			NewSuspended = orddict:erase({?DM,ID},
						     Suspended),
			
%		    io:format("SuspendedInfo: ~p~n",[SuspendedInfo]),
			process_dm_response(Agent#agentRationale{
					      suspended = NewSuspended},
					    Response,SuspendedInfo);
		    error-> %The suspended intention does not exist
		    
			io:format("[RC] No suspended action with id: ~p~n",
				  [{?DM,ID}]),
			Agent
		end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RESPONSES FROM SM
	    {?SM, {ID, Response}}-> 
		case  orddict:find({?SM,ID}, Suspended) of
		    {ok, SuspendedInfo} ->
			NewSuspended = orddict:erase({?SM,ID},
						     Suspended),
			
			process_sm_response(Agent#agentRationale{
					      suspended = NewSuspended},
					    Response,SuspendedInfo);
		    error-> %The suspended intention does not exist
			
			io:format("[RC] No suspended action with id: ~p~n",
				  [{?SM,ID}]),
			io:format("[RC] Response: ~p~n",
				  [Response]),
			
			Agent
		end;
		    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Monitor Notifications

	    {?SM, Notification = #monitor_notification{}} ->
		process_monitor_notification(Agent, Notification);
	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	    {external_action,ID,Result}->
		%%io:format("Match: ~p~n",[lists:keytake(ID,1,Suspended)]),
		
		{value,
		 {ID,RelatedIntention},
		 NewSuspended} = 

		    try
			lists:keytake(ID,1,Suspended)			
		    catch _:_ ->
			    io:format("[ReasoningCycle:processMessage,error]"++
					  "No suspended action with ID: ~p~n"++
					  "Suspended: ~p~n",[ID,Suspended]),
			    exit(error)		 
		    end,
                

	%	io:format("RelatedIntention: ~p~n",[RelatedIntention]),
	%	io:format("Suspended: ~p~n",[Suspended]),
	%	io:format("NewSuspended: ~p~n",[NewSuspended]),
	%	io:format("Result: ~p~n",[Result]),
		   % find_suspended_intention(Suspended,ID)
		case Result of
		    false -> %% External action not executed due to some error
			FEvent = 
			    findNextFailureEvent(
			      #event{relatedIntention = RelatedIntention}),
			applyChanges(
			  Agent#agentRationale{
			    suspended = NewSuspended},
			  [FEvent]);
		    
		    {ok,NewBindings}->
			
			[CurrentPIPlan|Rest] = RelatedIntention,
			
			Valuation = CurrentPIPlan#piPlan.valuation,
			NewValuation = variables:update(Valuation, 
							NewBindings),
			NewIntention = [CurrentPIPlan#piPlan{valuation =
							     NewValuation}|
					Rest],
			Change = furtherInstantiatePlan(NewIntention),
			applyChanges(Agent#agentRationale{
				       suspended = NewSuspended},
				     Change)
		end;
	     	

%%%%%%%%COMMUNICATION MESSAGES (tell, untell, achieve, askone....)

%%% TELL message

		    
	    {communication, Sender,_FromNode,{tell,TellContent}}->
		%% io:format("Original TellContent: ~p~n",[TellContent]),
		Now =  variables:make_timestamp_string(),
		Replacements =
		    variables:obtain_replacements(
		      "VARFROMTELL"++
			  Now++
			  "_",
		      2,[TellContent]),

		NewTellContent =
		    variables:use_replacements(TellContent, Replacements),

		%% io:format("New TellContent: ~p~n",[NewTellContent]),


		%% Add the new belief, with the annot source(Sender)
		try


		    NewEvent = utils:add_belief([],NewTellContent,Now,Sender),
		    %%Bindings not needed, because NewTellContent is fully valuated

		    %%io:format("NEWEVENT: ~p~n",[NewEvent]),
		    applyChanges(Agent,[NewEvent])



		catch
		    %% TODO: implement this check before, to save the hassle
		    %% of sending an invalid message.
		    %% e.g. .send(_,tell,[1,2,3]).
		    error:{case_clause,_} ->
			%%Content ignored
			Agent
		end;

	    {communication, Sender, _Arch, {achieve,AchieveContent}}->
		Now =  variables:make_timestamp_string(),
		Replacements =
		    variables:obtain_replacements(
		      "VARFROMACHIEVE"++
			  Now++
			  "_",
		      2,[AchieveContent]),

		NewAchieveContent =
		    variables:use_replacements(AchieveContent, Replacements),


		%% Add the new goal, with the annot source(Sender)
		try


		    NewEvent = utils:add_achievement_goal([],NewAchieveContent,Now,Sender),
		    %%Bindings not needed, because NewTellContent is fully valuated
		    
		     %% io:format("NEWADDACHEVENT: ~p~n",[NewEvent]),
		    applyChanges(Agent,[NewEvent])
		    
		    
		catch
		    %% e.g. .send(_,achieve[1,2,3]).
		    error:{case_clause,_} ->
			%% io:format("[DEBUG:] Bad formed achieve: ~p~n",
			%% 	  AchieveContent),
			%%Content ignored
			    Agent
		end;

	    
	    {communication, _Sender, _Arch, {untell,UntellContent}}->
		Now =  variables:make_timestamp_string(),
		Replacements =
		    variables:obtain_replacements(
		      "VARFROMUNTELL"++
		      Now++
		      "_",
		      2,[UntellContent]),
		
		NewUntellContent =
		    variables:use_replacements(UntellContent, Replacements),

		%% io:format("New TellContent: ~p~n",[NewTellContent]),


		%% Add the new belief, with the annot source(Sender)
		try
		    
		    NewEvent = utils:remove_belief([],
						   NewUntellContent),
		    %%Bindings not needed, because NewTellContent is fully valuated
		    
		    %%io:format("NEWEVENT: ~p~n",[NewEvent]),
		    applyChanges(Agent,[NewEvent])
	    
		catch
		    %% e.g. .send(_,untell,[1,2,3]).
		    error:{case_clause,_} ->
			%%Content ignored
			    Agent
		end;
		    {terminate,kill} ->
			Agent#agentRationale{events = [Message|Events]};
		    {terminate,kill,_TestPid} ->
			Agent#agentRationale{events = [Message|Events]}
		end,
		
		NewAgent.

%    processMessages(Agent,BB,List,[NewEvent|Acc]).


     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     %%
     %%  Processing Messages from the Distribution Manager
     %%
     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% PARAMS: Response that reactivates the suspended action
%%         {SuspendedRecord,SuspendedIntention}            
%%
%%  Recall that suspended actions are generated in actions:execute(...)
%%
%% Note: if protocol completed: furtherinstantiateplan + applychanges executed
%%       if failed, findNextFailureEvent + applyChanges
%%       if further steps required: update suspended info
%% 
%% Return: updated #agentRationale

%% TODO-> modify the return values to allow their inclusion in 
%%        plan contexts/ rules
process_dm_response(Agent,    %%%%%%%%%%%%% SUSPENDED SEND
		    #send_message_response{
		      result = Result,
		      found_in_container = Container
		     },
		    {#suspended_send{ my_name = MyName,
				     receiver_name = ReceiverName,
				     performative = Performative,
				     message = Message},
		     SuspendedIntention})->
    
    case Result of 
	?NOAGENT ->   
	    FEvent = 
		findNextFailureEvent(
		  #event{relatedIntention = SuspendedIntention}),
	    applyChanges(
	      Agent,
	      [FEvent]);
	
	?AGENTFOUND ->
	    %% io:format("~p Sending: ~p \nto: ~p~n",
	    %% 	      [Agent#agentRationale.agent_name,
	    %% 	       {communication,MyName,node(),
	    %% 		{Performative,Message}},
	    %% 		{ReceiverName,Container}]),
	    

	    
	    %% Send the message
	    {ReceiverName,Container} ! 
		{communication,MyName,node(),
		 {Performative,Message}},
	    Change = furtherInstantiatePlan(SuspendedIntention),
	    NewAgent = applyChanges(Agent, Change),
	    NewAgent
    end;

process_dm_response(Agent,    %%%%%%%%%%%%% SUSPENDED CONNECT
		    #connection_response{result = Result},
		    {#suspended_connect{},
		     SuspendedIntention})->
    %% io:format("[RC DEBUG:] Connection Response: ~p~n",[Result]),
    
    case Result of 
	?CONNECTED ->
	    Change = furtherInstantiatePlan(SuspendedIntention),
	    NewAgent = applyChanges(Agent,
				    Change),
	    NewAgent;
	_ -> % ?NAMECLASH,NOCONTAINER: no merge possible
	    FEvent = 
		findNextFailureEvent(
		  #event{relatedIntention = SuspendedIntention}),
	    NewAgent = applyChanges(
			 Agent,
			 [FEvent]),
	    NewAgent
    end;
process_dm_response(Agent,    %%%%%%%%%%%%% SUSPENDED DISCONNECT
		    #disconnection_response{},
		    {#suspended_disconnect{},
		     SuspendedIntention})->
    
    Change = furtherInstantiatePlan(SuspendedIntention),
    NewAgent = applyChanges(Agent,
			    Change),
    NewAgent;

process_dm_response(Agent,    %%%%%%%%%%%%% SUSPENDED CREATEAGENT
		    #create_agent_response{
		      result = Result},
		    {#suspended_create_agent{ agent_name = AgentName,
					      anonymousVar = AnonymVar,
					      use_bindings = UseBindings},
		     SuspendedIntention})->
    
    case Result of 
	?CREATED ->
	    %% io:format("[RC DEBUG:] Agent ~p created.~n",[AgentName]),

	    case AnonymVar of
		"" ->
		    Change = furtherInstantiatePlan(SuspendedIntention),
		    applyChanges(Agent,
				 Change);
		
		#var{} ->
		    %% ANONYMOUS agent creation
		    ResultVar =
			#var{functor = AgentName,
			     id = AgentName,
			     args = ?ISATOM,
			     annots = []},
		    
		    Bindings =
			orddict:store(
			  AgentName,
			  ResultVar,
			  UseBindings),
		    
		    
		    case variables:match_vars(Bindings,
					      AnonymVar,ResultVar) of
			false ->
			    FEvent = 
				findNextFailureEvent(
				  #event{relatedIntention = SuspendedIntention}),
			    NewAgent = applyChanges(Agent,[FEvent]),
			    NewAgent;
			ItNewBindings ->
			    %% Replace bindings in
			    %% suspended intention
			    Changes = processAnswer(SuspendedIntention,
						    {replace_bindings,ItNewBindings}),
			    
			    applyChanges(Agent, Changes)
		    end
	    end;

	_-> %%?NAMECLASH,?NOCONTAINER,?NOCODE 
	    
	    
	    io:format("[RC DEBUG:] Agent ~p not created.~n"++
		      "            Reason: ~p~n",[AgentName,Result]),


	    FEvent = 
		findNextFailureEvent(
		  #event{relatedIntention = SuspendedIntention}),
	    NewAgent = applyChanges(
			 Agent,
			 [FEvent]),
	    NewAgent
    end;


process_dm_response(Agent,    %%%%%%%%%%%%% SUSPENDED FINDAGENT
		    #find_agent_response{result = Result
					},
		    {#suspended_find_agent{},
		     SuspendedIntention})->
    case Result of 
	?NOAGENT ->
	    %% Agent not found, the execution fails
	    FEvent = 
		findNextFailureEvent(
		  #event{relatedIntention = SuspendedIntention}),
	    NewAgent = applyChanges(
			 Agent,
			 [FEvent]),
	    NewAgent;
    
	?AGENTFOUND ->
	    Change = furtherInstantiatePlan(SuspendedIntention),
	    applyChanges(Agent,
			 Change)		 
    
    end; 
process_dm_response(Agent,    %%%%%%%%%%%%% SUSPENDED FINDCONTAINER
		    Find = #find_agent_response{result = Result,
						container= ContainerName},
		    {Sus = #suspended_find_container{ container_var = ContainerVar,
						use_bindings = UseBindings},
		     SuspendedIntention = [PIP|Rest]})->

    %% io:format("[RC] FindAgentResp: ~p~nSuspendedFindCont: ~p~n",
    %% 	      [Find, Sus]),
    


    case Result of 
	?NOAGENT ->
	    %% Agent not found, the execution fails
	    FEvent = 
		findNextFailureEvent(
		  #event{relatedIntention = SuspendedIntention}),
	    NewAgent = applyChanges(
			 Agent,
			 [FEvent]),
	    NewAgent;
    
	?AGENTFOUND ->
	    %% Agent found in ContainerName
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
		    FEvent = 
			findNextFailureEvent(
			  #event{relatedIntention = SuspendedIntention}),
		    NewAgent = applyChanges(
				 Agent,
			 [FEvent]),
		    NewAgent;
		ItNewBindings ->
		    %% Replace bindings in suspended intention
		    %% NewIntention = 
		    %% 	[PIP#piPlan{ valuation = iterator:first(ItNewBindings)}
		    %% 	 |Rest],
		    %% Change = furtherInstantiatePlan(NewIntention),
		    Changes = processAnswer(SuspendedIntention,
					    {replace_bindings,ItNewBindings}),
		    
		    applyChanges(Agent, Changes)
	    end
    end;

process_dm_response(Agent,    %%%%%%%%%%%%% SUSPENDED Get Containers
		    #get_info_response{ containers= ReceivedContainers},
		    {#suspended_get_containers{ containers_var = ContainersVar,
						use_bindings = UseBindings},
		     SuspendedIntention = [PIP|Rest]})->


    %% Transform the list of containers into a variable
    ReceivedContainersVar =
	variables:erl_to_ejason(ReceivedContainers),
	%%TODO [term()] -> #var function is needed
	function_is_needed,

    %% io:format("[RC] Suspended get Containers. Containers received: ~p~n",
    %% 	      [ReceivedContainersVar]),
    
    NewVariables =
	variables:vars_to_import(ReceivedContainersVar),

    Bindings = variables:update(UseBindings, NewVariables),
    
    ResultVar =
	variables:get_var(ReceivedContainersVar#var.id, Bindings),

    case variables:match_vars(Bindings,ResultVar,ContainersVar) of
	false ->
	    FEvent = 
		findNextFailureEvent(
		  #event{relatedIntention = SuspendedIntention}),
	    NewAgent = applyChanges(
			 Agent,
			 [FEvent]),
	    NewAgent;
	ItNewBindings ->
	    Changes = processAnswer(SuspendedIntention,
				    {replace_bindings,ItNewBindings}),
	    
	    applyChanges(Agent, Changes)
    end;



process_dm_response(Agent, Response ,
		    {SuspendedAction,
		     _SuspendedIntention})->
    io:format("[RC] Wrong DM Response with Params: ~n"++%%Agent:~p~n"++
	      "Response: ~p~n"++
	      "SuspendedAction: ~p~n",
	      [%Agent,
	       Response,SuspendedAction]),
    Agent.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Processing Messages from the Supervision Manager
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% PARAMS: Response that reactivates the suspended action
%%         {SuspendedRecord,SuspendedIntention}            
%%
%%  Recall that suspended actions are generated in actions:execute(...)
%%
%% Note: if protocol completed: furtherinstantiateplan + applychanges executed
%%       if failed, findNextFailureEvent + applyChanges
%%       if further steps required: update suspended info
%% 
%% Return: updated #agentRationale
%%
%% TODO-> modify the return values to allow their inclusion in 
%%        plan contexts/ rules



%% The monitor was created. It is assessed whether the monitored
%% agent exists (generates an unknown_agent or agent_up notification) or
%% an existing relation is just updated
process_sm_response(Agent, #monitor_response{
			      result = ?UPDATED},
		    {#suspended_monitor{}, SuspendedIntention})->
    %% A monitoring relation was updated
    Change = furtherInstantiatePlan(SuspendedIntention),
    NewAgent = applyChanges(Agent, Change);
process_sm_response(Agent, #monitor_response{
			      monitored_agent = Monitored,
			      persistence = Persists,
			      result = Result},
		    {#suspended_monitor{}, SuspendedIntention})->
    
    Change = furtherInstantiatePlan(SuspendedIntention),
    NewAgent = applyChanges(Agent, Change),
    
    Notification =
	#monitor_notification{
	   id = "RCMADENOTIFICATION"++variables:make_timestamp_string(),
	   prior_notification = ?NOPRIORNOTIFICATION,
	   monitored_agent = Monitored,
	   persists = Persists,
	   notification = 
	       case Result of
		   ?NOAGENT ->
		       ?UNKNOWN;
		   ?EJASONOK ->
		       ?CREATEDNOTIFICATION
	       end},
    process_monitor_notification(NewAgent,
				 Notification);

			   
process_sm_response(Agent,    %%%%%%%%%%%%% SUSPENDED KILLAGENT
		    #kill_agent_response{result = _Result},
		    {#suspended_kill_agent{},
		     SuspendedIntention})->
   
    Change = furtherInstantiatePlan(SuspendedIntention),
    NewAgent = applyChanges(Agent, Change),
    NewAgent;   

process_sm_response(Agent,    %%%%%%%%%%%%% SUSPENDED DEMONITOR
		    
		    #demonitor_response{result = ?EJASONOK,
					prior_notification = PriorNotification},
		    {#suspended_demonitor{monitored_agent=Monitored},
		     SuspendedIntention})->
    %% All agent_up(Monitored) and agent_down(Monitored)[reason(PN)]
    %% must be erased


    %% TODO: automatise all these transformations
    AgentChange =
	case PriorNotification of
	    ?CREATEDNOTIFICATION ->
		variables:erl_to_ejason(agent_up);
	    _ ->
		variables:erl_to_ejason(agent_down)
	end,
    
    MonitoredVar = variables:erl_to_ejason(Monitored),
    ReasonVar = variables:erl_to_ejason(reason),
    TimeStamp = variables:make_timestamp_string(),
    PriorVar = variables:erl_to_ejason(PriorNotification),
    PerceptVar = variables:erl_to_ejason(percept),
    SourceVar = variables:erl_to_ejason(source),
    SourcePercept = 
	#var{id = list_to_atom("Source"++TimeStamp),
	 functor = {source},
	 args = {{percept}},
	 annots = []},   
    
     
    DeletedStruct =
	case PriorNotification of
	    ?CREATEDNOTIFICATION ->
		%% agent_up(monitored)
		#var{id = list_to_atom("PriorAgentUp"++TimeStamp),
		     functor={agent_up},
		     args={{MonitoredVar#var.id}},
		     annots = []};
	    _ ->
		%% reason(Prior)
		#var{id = list_to_atom("PriorDownReason"++TimeStamp),
		     functor = {reason},
		     args = {{PriorVar#var.id}},
		     annots = []}
	end,
    
    RemovedBelief =
	case  PriorNotification of
	    ?CREATEDNOTIFICATION ->
		%% agent_up(Monitored)[source(percept)]
		#var{id = list_to_atom("RemoveMonitorBelief"++TimeStamp),
		     functor = {AgentChange#var.id},
		     args = {{MonitoredVar#var.id}},
		     annots = [{SourcePercept#var.id}]};
	    _ ->
		%% agent_down(Monitored)[reason(PriorNotif), source(percept)]
		#var{id = list_to_atom("RemoveMonitorBelief"++TimeStamp),
		     functor = {AgentChange#var.id},
		     args = {{MonitoredVar#var.id}},
		     annots = [{DeletedStruct#var.id}, {SourcePercept#var.id}]}
	end,

    NewVars =
	[RemovedBelief, MonitoredVar, ReasonVar, PriorVar, PerceptVar,
	 SourceVar, SourcePercept, DeletedStruct, AgentChange],
    
    
    Bindings =
	variables:update(orddict:new(),
			 NewVars),
    
    RemoveBeliefEvent =
       	utils:remove_belief(Bindings, RemovedBelief),


    
    Changes = furtherInstantiatePlan(SuspendedIntention),
    NewAgent = applyChanges(Agent, [RemoveBeliefEvent|Changes]),
    NewAgent; 


process_sm_response(Agent,    %%%%%%%%%%%%% SUSPENDED SUPERVISE AGENTS
		    #supervision_response{result = Result},
		    {#suspended_supervise{},
		     SuspendedIntention})->
 
    case Result of 
	?EJASONERROR ->
	    %% Supervision relation not created, the execution fails
	    FEvent = 
		findNextFailureEvent(
		  #event{relatedIntention = SuspendedIntention}),
	    NewAgent = applyChanges(
			 Agent,
			 [FEvent]),
	    NewAgent;
    
	?EJASONOK ->
	    Change = furtherInstantiatePlan(SuspendedIntention),
	    applyChanges(Agent,
			 Change)		 
    
    end; 

process_sm_response(Agent, Response ,
		    {SuspendedAction,
		     _SuspendedIntention})->
    io:format("[RC] Wrong SM Response with Params: ~n"++
	      "Response: ~p~n"++
	      "SuspendedAction: ~p~n~n",
	      [Response, SuspendedAction]),
    Agent.




%% MONITOR NOTIFICATIONS (unknown, unreachable, restart, revive, dead, created)
%% Deletes the prior notification (if any) and generates the new one. This way,
%% a monitoring agent does not believe a monitored agent to be simultaneously 
%% dead/unreachable... (at least labeled source(percept) )
    
process_monitor_notification(
  Agent, #monitor_notification{
	    id = NotificationID,
	    monitored_agent = Monitored,
	    prior_notification = Prior,
	    notification = Notification,
	    persists = RelationPersists})->
    
    TimeStamp =variables:make_timestamp_string(),
    
    %% io:format("[RC ~p] Notification ~p received: ~p~n Monitored: ~p "++
    %% 		   "Prior: ~p~n Persists: ~p~n",
    %% 	       [Agent#agentRationale.agent_name, NotificationID,Notification,
    %% 	       Monitored, Prior, RelationPersists]),
    
				     

    %% Construct the struct agent_down(Monitored)[reason(Notification),
    %% source(percept)]
    %% or agent_up(Monitored)[source(percept)]


    %% TODO: try to create just one variable manually and then automate the variable extraction
    AgentDown = variables:erl_to_ejason(agent_down),

    AgentUp = variables:erl_to_ejason(agent_up),

    AgentVar = variables:erl_to_ejason(Monitored),

    ReasonVar = variables:erl_to_ejason(reason),

    NotificationVar = variables:erl_to_ejason(Notification),

    SourceVar = variables:erl_to_ejason(source),
    
    PerceptVar = variables:erl_to_ejason(percept),

    SourcePercept =
    	#var{id = list_to_atom("Source"++TimeStamp),
    	     functor = {source},
    	     args = {{percept}},
    	     annots = []},   

    PriorVar = variables:erl_to_ejason(Prior),

    ReasonStruct =
	#var{id = list_to_atom("DownReason"++TimeStamp),
	     functor = {reason},
	     args = {{NotificationVar#var.id}},
	     annots = []},
    
   
    %% Prior notification to be deleted
    PriorStruct =
	case Prior of
	    ?CREATEDNOTIFICATION ->
		%% agent_up(monitored)
		#var{id = list_to_atom("PriorAgentUp"++TimeStamp),
		     functor={agent_up},
		     args={{AgentVar#var.id}},
		     annots = []};
	    _ ->
		%% reason(Prior)
		#var{id = list_to_atom("PriorDownReason"++TimeStamp),
		     functor = {reason},
		     args = {{PriorVar#var.id}},
		     annots = []}
	end,
    

		

    RemovedBelief =
	case  Prior of
	    ?CREATEDNOTIFICATION ->
		%% agent_up(Monitored)[source(percept)]
		#var{id = list_to_atom("RemoveMonitorBelief"++TimeStamp),
		     functor = {AgentUp#var.id},
		     args = {{AgentVar#var.id}},
		     annots = [{SourcePercept#var.id}]};
	    
	    _ ->
		%% agent_down(Monitored)[reason(Prior), source(percept)]
		#var{id = list_to_atom("RemoveMonitorBelief"++TimeStamp),
		     functor = {AgentDown#var.id},
		     args = {{AgentVar#var.id}},
		     annots = [{PriorStruct#var.id}, {SourcePercept#var.id}]}
	end,
    


    
    NewBelief =
	case Notification of
	    ?CREATEDNOTIFICATION ->
		#var{ id = list_to_atom("NewMonitorBelief"++TimeStamp),
		      functor = {AgentUp#var.id},
		      args ={{AgentVar#var.id}},
		      annots = []};
	    _ ->      
		#var{id = list_to_atom("DownBelief"++TimeStamp),
		     functor = {AgentDown#var.id},
		     args = {{AgentVar#var.id}},
		     annots = [{ReasonStruct#var.id}]}
	end,



    DemonitorBelief =
	NewBelief#var{
	  id = list_to_atom("DemonitorBelief"++TimeStamp),
	  annots =
	      [{SourcePercept#var.id}|NewBelief#var.annots]
	 },
    
    

   %% If monitor persists, do not add demonitorbelief

    Vars = [NewBelief,RemovedBelief,ReasonStruct, PriorStruct, PriorVar,
	    SourceVar, PerceptVar, SourcePercept, NotificationVar,
	    AgentDown,AgentUp, AgentVar,ReasonVar],
    
    NewVars = case RelationPersists of 
		  true ->
		      Vars;
		  false ->
		      [DemonitorBelief|Vars]
	      end,
		
    Bindings =
	variables:update(orddict:new(),
			 NewVars),


    %% Add new belief, but delete whatever other
    %% "agent_down(Monitored)" there existed! So that a monitoring
    %% agent does not believe another thing.

    %% io:format("[RC ~p] Prior Notification: ~p~n",
    %% 	      [Agent#agentRationale.agent_name,  Prior]),
    

    RemoveBeliefEvent =
	case Prior of
	    ?NOPRIORNOTIFICATION ->
		?NOEVENT;
	    _ ->
		utils:remove_belief(Bindings,RemovedBelief)
	end,

    %% Monitoring notifications get appended the label [source(percept)]
    AddBeliefEvent = 
	utils:add_belief(Bindings, NewBelief, TimeStamp, percept),


    %% If the monitoring relation does not persist, then erase the 
    %% new belief added (however, a notification will still be received)

    DemonitorBeliefEvent =
       	utils:remove_belief(Bindings, DemonitorBelief),


  


    %% io:format("[RC] AddBeliefEvent: ~p~n",[AddBeliefEvent]),


    Events = case RelationPersists of
		 true ->
		     [RemoveBeliefEvent, AddBeliefEvent];
		 false ->
		     [RemoveBeliefEvent, AddBeliefEvent, DemonitorBeliefEvent]
	     end,
    			 

    %% io:format("[RC ~p] Removing Belief: ~p~n",
    %% 	      [Agent#agentRationale.agent_name, 
    %% 	       variables:valuate(Bindings, RemovedBelief)]),

    %% io:format("[RC ~p] Remove Belief Event: ~p~n",
    %% 	      [Agent#agentRationale.agent_name, 
    %% 	       RemoveBeliefEvent]),

    %% io:format("[RC ~p] Add Belief Event: ~p~n",
    %% 	      [Agent#agentRationale.agent_name, 
    %% 	       AddBeliefEvent]),
   
    applyChanges(Agent,Events).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  PLAN FAILURE HANDLING
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%TODO: erase duplicities


% Generates the failure event corresponding to some event
% By now, only for failed goal addition plans.
make_failure_event(Event = #event{type=Type})->
%% io:format("MakeFailEvent: Event: ~p~n",[Event]),
    case Type of
	%% ?FAILEDACHGOAL ->
	%%     Event;
	%% ?FAILEDTESTGOAL->
	%%     Event;
	?ADDACHGOAL ->
	    Event#event{type= ?FAILEDACHGOAL};
	?ADDINTENTIONGOAL ->
	    Event#event{type= ?FAILEDACHGOAL};
	?ADDTESTGOAL ->
	    Event#event{type= ?FAILEDTESTGOAL};
	_->
	    findNextFailureEvent(Event)
%        add_ejason_private_query ->
    
%	_Other ->
%	    io:format("[reasoningCycle.erl] Warning: trying to get"++
%		      " failure plan for an event of type ~p~n.",[Type]),
%	    ?NOEVENT
    end.


%% Identifies the intended means (plan) for the chosen event.
%% 
findIntendedMeans(_AgentInfo,Event = [],_Plans,_OptionSelector) ->
    [];
findIntendedMeans(AgentInfo,Event,Plans,OptionSelector) ->
    IntendedMeans =
	case Event of 
	    {terminate,kill}->
		{terminate,kill};
	    {terminate,kill,Test}->
		{terminate,kill,Test};
	    _ ->
		%%io:format("Finding IntendedMeans~n"),
		ItApplicablePlans =
		    findApplicablePlans(AgentInfo,Event,Plans),
		%io:format("~n~nALL APPLICAPLANS: ~p~n~n~n",
		%	  [iterator:get_all(ItApplicablePlans)]),
		OptionSelector(ItApplicablePlans)
	end,

    %% io:format("IntendedMeans: ~p~n~n~n~n",[IntendedMeans]),
    case {Event, IntendedMeans} of
	{Event = #event{
		    %% This event must be retried. 
		    type = ?ADDWAITTESTGOAL,
		    body = Query,
		    relatedIntention = [CurrentPiPlan|Rest]
		   },
	 []}->
	    %% Will again execute the formula ??Query  
	    Valuation  =
	    	(CurrentPiPlan#piPlan.valuation),
	    %% io:format("[RC findIntended] The current Valuation: ~p~n",
	    %% 	      [Valuation]),

	    case actions:wait_test_goal(AgentInfo, Valuation, Query) of
		#event{} ->
		    %% The query cannot be matched from the BB, it will be
		    %% retried in the future
		    [];
		{replace_bindings, ItBindings} ->	    
		    NewValuation = iterator:first(ItBindings),
		    %% io:format("[RC findIntended] The new Valuation: ~p~n",
		    %% 	      [NewValuation]),
			      	      
		    NewIntention = 
			[CurrentPiPlan#piPlan{valuation =
						  NewValuation}|Rest],
		    case furtherInstantiatePlan(NewIntention) of
			[{deleteIntention, []}] ->
			    %% The intention was completely executed. Event
			    %% replaced to just drop the intention.
			    [];
			[{addIntention, UseIntention}] ->
			    %% The query was matched, then add the resulting
			    %% intention
			    {?ADDINTENTION,
			     UseIntention}
		    end
	    end;
	_ ->%Intended Means (plan) found. 
	    IntendedMeans
    end.


%% %% Looks for the intended means for a failure event given as input.
%% %% It not found, generates failure events for the intention stack
%% %% and iterates. 
%% findFailureIntendedMeans(_Agent,?NOEVENT,_Plans,_OptionSelector)->
%%     %io:format("Drop Intention: ~p~n",[DropIntention]),
%%     % No failure plan found
%% %    DropIntention;
%%     {?NOEVENT,[]};
%% findFailureIntendedMeans(Agent,FailureEvent,Plans,OptionSelector)->
%%    %%  FailureEvent = make_failure_event(Event),
%%    %% io:format("FEvent: ~p~n",[FailureEvent]),
%%     ItFailureApplicablePlans = 
%% 	findApplicablePlans(Agent,
%% 			    FailureEvent,Plans),
%%     case OptionSelector(ItFailureApplicablePlans) of
%% 	[] ->
%%     %% If the Event has some related intention, the plan on top of it
%%     %% fails.
%% 	    NewFailureEvent=findNextFailureEvent(FailureEvent),
 
%% 	    findFailureIntendedMeans(Agent,NewFailureEvent,
%% 				     Plans,OptionSelector);
%% 	%	    end;		 
%% 	PlanforIntendedMeans ->
%% %	    io:format("PlanForFailure: ~p~n",[PlanforIntendedMeans]),
%% 	    {FailureEvent, PlanforIntendedMeans}
%%     end.



% Generates the proper failure event from the intention stack
% Only plans for goal addition (test and achievement) may have failure plans
findNextFailureEvent(_Event =  #event{relatedIntention = []})->
    %% No upper-level failure plan can be generated. 
    %%The intention will be dropped
    ?NOEVENT;
findNextFailureEvent(#event{relatedIntention =
			    RelatedIntention})->
    %% [First|_] = RelatedIntention,
    %% io:format("First: ~p~n",[First#piPlan.init_bindings]),

    %% io:format("Related: ~p~n",[RelatedIntention]),
    
    %% Make failure event for the plan on top of the related intention (a stack,
    %% failures are propagated downwards)
    [#piPlan{event = Event}
     |Rest] = RelatedIntention,
    
    %% io:format("FailurePlan for : ~p~n",[Event]),
    
  %%  timer:sleep(10000),
    case Event#event.type of
   	?ADDACHGOAL -> %only plans for achievement/test goals can fail
    	    FEvent = Event#event{ type = ?FAILEDACHGOAL,
				  relatedIntention = Rest},
    	%	    io:format("Next Failure Event: ~p~n",[FEvent]),
    	    FEvent;
    	?ADDTESTGOAL -> %only plans for achievement/test goals can fail
    	    FEvent = #event{ type = ?FAILEDTESTGOAL,
    			     relatedIntention = Rest},
    	%	    io:format("Next Failure Event: ~p~n",[FEvent]),
    	    FEvent;
    	_ -> % cannot fail, try with next
    	    findNextFailureEvent(#event{relatedIntention = Rest})
    end.
