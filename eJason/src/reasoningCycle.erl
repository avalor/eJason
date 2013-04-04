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
%% @doc Implements the reasoning cycle of an eJason agent

-module(reasoningCycle).

-export([start/5,reasoningCycle/1, applyChanges/2,selectEvent/1,
	selectPlan/1,selectIntention/1]).
%-compile(export_all).


-define(NULL, []).
-include("ejason.hrl").
-include("macros.hrl").
-include("parser.hrl").
-include("variables.hrl").
-include("dm_requests.hrl").
-include("sup_requests.hrl").
-include("suspendedActionsRecords.hrl").


% Initializes an agent mental state
start(AgentName,EnvironmentName,InitialEvents, Plans,BB)->
    #agentRationale{agentName = AgentName,
		    plans = Plans,
		    environment = EnvironmentName,
		    belief_base = BB,
		    events = InitialEvents}.


reasoningCycle(#agentRationale{ events = [],
				info = {Iterations,WaitTime,_MChecked},
				intentions = []}= Agent)->
    {NewIterations,NewWaitTime} = 
	if 
	    WaitTime >= 1000 ->
		{0,WaitTime};
	    true ->
		if 
		    Iterations >= 10 ->
			{0,WaitTime *2};
		    true ->
			{Iterations +1,WaitTime}
		end
	end,
    
    timer:sleep(NewWaitTime), %% Comment to skip process sleep time
%    io:format("Slept ~p milliseconds~n",[NewWaitTime]),
    NewAgent = check_mailbox(Agent),
    reasoningCycle(NewAgent#agentRationale{info={NewIterations,NewWaitTime,
						?MBOXCHECKED}});
reasoningCycle(Agent= #agentRationale{info={_NewIterations,_NewWaitTime,
						MailCheck}})->
   %io:format("Initial Agent: ~p~n",[Agent]),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %STEP #1: check mailbox, if not just done
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    NewAgent = case MailCheck of
		   ?MBOXCHECKED->
		       Agent;
		   ?MBOXNOTCHECKED -> 
		       check_mailbox(Agent)
	       end,
    
    #agentRationale{selectEvent = EvSel, 
		    events = Events,
		    agentName = AgentName,
		    belief_base = BB,
		    module_name = ModuleName,
		    plans = Plans,
		    intentions = Intentions,
		    selectPlan = OptionSelector,
		   % suspended = SuspendedIntentions,
		    selectIntention = IntentionSelector} =
	NewAgent,
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
   %STEP #2: choose one event using the event selection function
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
   {Event, NotChosenEvents} = EvSel(Events),


%  case AgentName of
 %     owner ->
%	  io:format("~p BB: ~p~n",[AgentName,BB]);
%	  io:format("~p Selected Event: ~p~n",[AgentName,Event]),
 %     _ -> 
	  
  %{}
% end,
    
      
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
   %STEP #3: find intended means for the chosen event. That event 
   %         may be replaced by a failure event.
   %
   % The intended means are a plan record where the return params and
   % the variables in the trigger and the context may be already bound
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {NewEvent, IntendedMeans} = 
	findIntendedMeans(NewAgent,Event,Plans,OptionSelector),

    
% io:format("Agent: ~p INTENDED MEANS: ~p~n",[AgentName,IntendedMeans]),
%  io:format("INTENDED body: ~p~n",[IntendedMeans#plan.body]),
 % io:format("INTENDED bindings: ~p~n",[IntendedMeans#plan.bindings]),
  %io:format("INTENDED return: ~p~n",[IntendedMeans#plan.return_variables]),

 %   io:format("NewEvent: ~p~n",[NewEvent]),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %STEP #4: process intended means (create new/update intention stack..)
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    AllIntentions = 
	processIntendedMeans(NewEvent,Intentions,IntendedMeans),
 % io:format("All Intentions: ~p~n",[AllIntentions]),
    


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %STEP #5: choose one intention to execute. Special cases for agent
    %         termination are considered.
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    case IntentionSelector(AllIntentions) of
	{{terminate,kill},_}->
	    io:format("[DEBUG:] Agent ~p killed.~n",[Agent#agentRationale.agentName]);
	{{terminate,kill,TestPid},_} when is_pid(TestPid)->
	    TestPid ! {killed, #agentRationale.agentName};
	    %io:format("Agent ~p killed. Answer sent to ~p~n",
	%	      [Agent#agentRationale.agentName,TestPid]);
	[] ->
	   % reasoningCycle(Agent);
	    reasoningCycle(Agent#agentRationale{events = NotChosenEvents,
					       info={0,1,?MBOXNOTCHECKED}});
	
	{Intention,NotChosenIntentions} ->
%	    io:format("Chosen Intention: ~p~n",[Intention]),
	    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %STEP #6: execute the intention.
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	    Result = executeIntention(BB,ModuleName,Intention),
%	   io:format("Result from Execute intention is ~p~n",[Result]),


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %STEP #7: apply the proper changes to the mental state with 
    %         respect to the last in.
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	    FinalAgent = 
		applyChanges(Agent#agentRationale
			     {events = NotChosenEvents,
			      intentions = NotChosenIntentions},
				     Result),
%	   io:format("FinalAgent: ~p~n",[FinalAgent]),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %STEP #8: iterate the reasoning cycle with the new mental state.
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


	   reasoningCycle(FinalAgent#agentRationale{
			    info={0,1,?MBOXNOTCHECKED}});%% Info is RESTARTED
	Other->
	    io:format("[RC for ~p] error: Intention would be: ~p~n",
		      [AgentName,Other])
    end.





% Generates an iterator that can compute all applicable plans
findApplicablePlans(Agent = #agentRationale{belief_base = BB},
		    #event{type = Type,body=EBody},PlanList)->
% io:format("PlanList: ~p~n",[PlanList]),
%	  io:format("BB: ~p~nType: ~p~nBody: ~p~nPlan: ~p~n",
 %   [BB,Type,EBody,PlanList]),

  %  io:format("FindAppPlans: EVENTBODY: ~p~n",[EBody]),
    ItPlans = iterator:create_iterator(PlanList),
    case Type of
	add_ejason_private_query->
	    case private_query:resolve_query(Agent,EBody) of
		false ->

		    iterator:create_iterator([]);
		{[false],_}->
		    iterator:create_iterator([]);	
		{NewBindings,RetParams} ->
		  %  io:format("R: ~p~n",[R]),
		    iterator:create_iterator(
		      [#plan{bindings = NewBindings,
			     return_variables = {RetParams,[]},
			     body = [fun private_query:formula/1]}])
	    
	    end;

	_ ->
	    Fun =  fun (Plan) -> match_plan(BB,Type,EBody,Plan) end,
	    iterator:create_iterator_fun(ItPlans,Fun)
    end.



match_plan(BB,EventType,EventBody,% = {Name,Args,_Annot},
	   Plan = #plan{trigger=Trigger,bindings=Bindings}) ->
%   io:format("EventType: ~p~nEvent: ~p~nPlan: ~p~n",
%	      [EventType,EventBody,Plan]),
% io:format("EventBodyList: ~p\n",
%	      [tuple_to_list(EventBody)]),

    Params  = 
	case tuple_to_list(EventBody) of
	    [Name,?FAILEDTESTGOAL,_ModuleName,Args,Annot] when is_tuple(Args)->
		[BB,Bindings,Name,EventType]++tuple_to_list(Args)++[Annot];
	    [Name,EventType,Args,Annot] when is_tuple(Args)->% achievement goal
		[BB,Bindings,Name,EventType]++tuple_to_list(Args)++[Annot];

	    [Name,Args,Annot] when is_tuple(Args)-> %add/remove belief
		[BB,Bindings,Name,EventType]++tuple_to_list(Args)++[Annot];
	    
	    [Name,EventType,ModuleName,Args,_Annot] -> % Test goal
		[BB,Bindings,Name,EventType,ModuleName,Args];
	    _Else ->
		io:format("[RC]Error: wrong event body: ~p~n",[EventBody]),
		exit(error)
	end,

%io:format("Params: ~p~n",[Params]),
%exit(avalor),
	%case Args of
	 %   {} ->
	%	[];
	 %   _ ->
%		tuple_to_list(Args)
%	end,
  %  io:format("Trigger: ~p~nParams: ~p~nLength: ~p~n",
%	      [Trigger,Params,length(Params)]),
    try
	case apply(Trigger,Params) of
	    false -> false; % The plan is not applicable
	    ItContext when is_function(ItContext) -> % The trigger matches
	    %io:format("Matched Trigger: ~p~nParams: ~p~n",
	    %  [Trigger,Params]),
	%	timer:sleep(1000),
		%exit(avalor),
%		io:format("ItContext genera: ~p~n",
%			  [iterator:get_all(ItContext)]),
		ContextFun = 
		    fun ({RetBindings,NewBindings}) -> %% not a test goal  
			   %io:format("AB: ~p~n",[A]),
			    
			    %NewParams = 
			%	variables:valuate_params(RetParams,NewBindings),
%			    io:format("NewParams: ~p~n",[NewParams]),
		
%	    io:format("Returned Params: ~p~n",[RetBindings]),

		%	    FinalBindings =
		%		variables:update(NewBindings, NewParams),
			    Plan#plan{bindings = NewBindings,
				     return_variables = RetBindings}
	%		(NewBindings) when is_list(NewBindings) ->%TestGoal
	%		    FinalBindings =
	%			variables:update(Bindings, NewBindings),
	%		    Plan#plan{bindings = FinalBindings}
				;
			(A) ->
			    io:format("[reasoningcycle]error: match_plan(~p)~n",[A]),

			    exit(b)
		    end,
		iterator:create_iterator_fun(ItContext,ContextFun);
	    Other ->
		io:format("[RC]error: match_plan1(~p)~n",[Other])

	end
    catch
	error:{badarity,_Error}-> %% The plan is not applicable
%	    io:format("Error: badarity~n",[]),
	    false% ;
%	  A:B ->
%	    io:format("[RC] Unexpected error: ~p:~p~n \n\n",[A,B]),
%	    exit(avalor)
    end.


    
	
%
%
%% Intention = [{Event,Plan,Valuation,[{FormulaFunction,Bindings}] }
%% Last event and plan are included to enhance observational power of
%% the intention selection function.


%%
%% Generates a new intention ([#piplan]) from the intended means
%% or updates an existing one
%%
processIntendedMeans(_Event,Intentions,[]) ->
    Intentions;
processIntendedMeans(_Event,Intentions,{terminate,kill}) ->
    I = [{terminate,kill}|lists:reverse(Intentions)], 
    %%Terminate only when there are no more intentions
    lists:reverse(I);
processIntendedMeans(_Event,Intentions,{terminate,kill,TestPid}) ->
    I = [{terminate,kill,TestPid}|lists:reverse(Intentions)], 
    %%Terminate only when there are no more intentions
    lists:reverse(I);
processIntendedMeans(Event,Intentions,
		     #plan{body = FormulaList,
			   bindings = Bindings,
			   return_variables = {RetParams,RetAnnots}}=Plan) ->
    
%    io:format("EventType: ~p~n",[Event#event.type]),
				     %%Remove initial bindings
 %   io:format("Removing bindings: ~p~nRetFormulas: ~p~n",
%	      [Bindings,Plan#plan.return_params]),

    % Param bindings necessary to generate the plan failure event
%    io:format("RetParams: ~p~n",[RetParams]),

    InitParams =  variables:valuate_params(RetParams, Bindings),

 %   io:format("RetAnnots: ~p~n",[RetAnnots]),
    InitAnnots = variables:valuate_annots(RetAnnots, Bindings),
  %  io:format("InitAnnots: ~p~n",[InitAnnots]),

    InitBindings = {InitParams,InitAnnots},
    
   % io:format("InitBindings: ~p~n",[InitBindings]),
    


    NewIntention =  
	case Event#event.relatedIntention of
	    []->
		[#piPlan{ event = Event,
			  plan = Plan#plan{bindings = []},
			  valuation = Bindings,
			  init_bindings = InitBindings,
			  formulas = FormulaList}];
						% IM create a new intention
	    RIntention->
		NewSIPlan = #piPlan{ event = 
				     Event#event{relatedIntention = []},

				     plan = Plan#plan{bindings = []},
				     valuation = Bindings,
				     init_bindings = InitBindings,
				     formulas = FormulaList},
		[NewSIPlan|RIntention]
	end,
    [NewIntention|Intentions].


%% Executes the intention chosen
%% Returns a list of state updates (e.g. adding new events 
%% or updating intention list)
executeIntention(_BBID,_ModuleName,[])->
    io:format("No intention executable"),
    [];
executeIntention(_BBID,ModuleName,Intention = [SIPlan|_RestSIP])->
%    io:format("Executing Intention: ~p~n",[Intention]),

    Valuation = SIPlan#piPlan.valuation,

    case SIPlan#piPlan.formulas of
	
	[Formula|_] ->
%	    io:format("Formula: ~p ~p~n",[Formula,Valuation] ),
	    Result = Formula(Valuation),
%	    io:format("Result of execution: ~p~n",[Result]),
	    processAnswer(ModuleName,Intention, Result);

	[] ->
%	    io:format("No more Formulas\n"),
	    case furtherInstantiatePlan(Intention) of
		[{addIntention,NewIntention}] ->
	    
%		    io:format("Finally adding Intention: ~p~n", 
		%	      [NewIntention]),

		    executeIntention([],ModuleName,NewIntention);
		[{deleteIntention,[]}] ->
		    [{deleteIntention,[]}]
	    end
    end.
%    io:format("AnswerList: ~p~n",[Result]),
 %   processAnswer(ModuleName,Intention, Result).



%% Updates the valuation of the PIP with the new bindings in the 
%% obtained valuations 
%% Removes the last executed formula from the body of the PIP 
%% and removes the PIP after the execution of the last formula.
%%
%% Returns either {deleteIntention, []} or {addIntention,_}
furtherInstantiatePlan([])->  
    [{deleteIntention,[]}];
furtherInstantiatePlan(
  [PIP = #piPlan{formulas = Formulas,
		 valuation = Bindings,
		 plan=#plan{return_variables = {RetParams,
					    RetAnnots}}}|RestPIP])->
%    io:format("Instantiate with return_params: ~p~n",[RetParams]),
     %  {[NewValuation],RestFormulas} = 
  %  io:format("Formulas: ~p~n",[Formulas]),
    NewIntention = 
	case Formulas of
	    [] ->
%		io:format("RestSip: ~p~n",[RestSIP]),
		case RestPIP of 
		    [] -> % No more plans in the intention
			[{deleteIntention,[]}]; 
		    [NextPIP = #piPlan{formulas = _NextFormulas,
				       valuation = OldBindings}|Rest] ->
	%    	io:format("Params: ~p~nBindings: ~p~n",[Params,Bindings]),
			%% Valuates the return params
			NewParams = 
			    variables:valuate_params(RetParams++RetAnnots,
						     Bindings),
			%io:format("NewParams: ~p~n",[NewParams]),
			NewValuation = 
			    variables:update(OldBindings,NewParams),
		        %io:format("NewValuation: ~p~n",[NewValuation]),
			case furtherInstantiatePlan(
			  [NextPIP#piPlan{valuation = NewValuation}|
			   Rest]) of
			    [{addIntention,NI}] ->
				NI;
			    [{deleteIntention,[]}] ->
				[{deleteIntention,[]}]
			end
		end;
    
	    [_ExecutedFormula|NextFormulas] ->
		[PIP#piPlan{formulas = NextFormulas}|RestPIP]
	end,

%    io:format("NewIntention: ~p~n",[NewIntention]),

    case NewIntention of 
	[{_,[]}] ->
      % io:format("NewIntention at last: ~p~n",[NewIntention]),

	    [{deleteIntention,[]}];
	_ ->
	    
	    [{addIntention,NewIntention}]
    end.
		       


%processAnswer(_IntentionExecuted,[],Acc)->
%    Res = lists:reverse(Acc),
%    io:format("Changes after Answers: ~p~n",[Res]),
%    Res;

%% Process the answer obtained during the last intention execution
%% Returns a list of the changes to the mental state of the agent
processAnswer(ModuleName,Intention,Answer)->
%  io:format("Processing ANSWER: ~p~n",[Answer]),
    
    Result = case Answer of
%		 {finished, NewValuation}->% SIP completely executed
%		     furtherInstantiatePlan(Intention,NewValuation);
		 #event{type = ?ADDACHGOAL,
		       body = {PlanName,Args,Annot}}->
		     [#event{type=add_achievement_goal,
			     body = {PlanName,add_achievement_goal,
				     Args,Annot},
			     relatedIntention=Intention}];%++
%			 furtherInstantiatePlan(Intention,[]);
		 #event{type = ?ADDTESTGOAL,
			body = {PlanName,Args,Annot}}->
		     [#event{type= add_test_goal, 
			     body = { PlanName,add_test_goal,
				      ModuleName, Args,Annot},
			     relatedIntention=Intention}];%++
%			 furtherInstantiatePlan(Intention,[]);
		 {add_ejason_private_query, {QueryName,Args,Annot}}->
		     [#event{type = add_ejason_private_query,
			     body ={ QueryName,
				     add_ejason_private_query,
				     ModuleName, 
				     Args,Annot},
			     relatedIntention=Intention}];
		 
		 {stutter_action} ->
		 	 furtherInstantiatePlan(Intention);

		 {suspend,DelegatePID,TimeStamp}->
		     [{suspend,Intention,{DelegatePID,TimeStamp}}];
		 

		 {?ACTIONSUSPEND,ID,SuspendedAction}-> % interact. ?DM or ?SM
		     [{?ACTIONSUSPEND,ID,SuspendedAction,Intention}];

		 {update_bindings,NewBindings} ->
		     % Typically from a binary operation 
		     [CurrentPIPlan|Rest] = Intention,
		     Valuation = CurrentPIPlan#piPlan.valuation,
		     NewValuation = variables:update(Valuation, 
						     NewBindings),
		     NewIntention = [CurrentPIPlan#piPlan{valuation =
							  NewValuation}|
				     Rest],
		     furtherInstantiatePlan(NewIntention);
		 
		 #event{type = ?ADDBELIEF}->
		     [Answer|furtherInstantiatePlan(Intention)];
		 #event{type = ?REMOVEBELIEF}->
		     [Answer|furtherInstantiatePlan(Intention)];
		 #event{type = ?REMOVEADDBELIEF}->
		     [Answer|furtherInstantiatePlan(Intention)];
		 #event{type = ?ADDINTENTIONGOAL,
			body = {PlanName,Args,Annot}}->
		     [#event{type=add_achievement_goal,
			     body = {PlanName,add_achievement_goal,
				     Args,Annot}}]++
		      furtherInstantiatePlan(Intention);
		 {fail}->
		     [#piPlan{event = Event}|Rest] = Intention,
		     FEvent = make_failure_event(Event#event{
						   relatedIntention = Rest}),
		     [FEvent];  			 
		     		      
		 _ ->
		     io:format("[RC, exit] Unknown Answer: ~p~n",[Answer]),
		     exit(avalor),
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
			     intentions = Intentions,
			     belief_base = BB,
			     suspended = Suspended}=Agent,
	     [Change |Changes])->

    %  io:format("Change: ~p~n",[Change]),

    NewAgent = case Change of
		   {deleteIntention,_}->
		       Agent;
		   {addIntention,NewIntention} ->
		    %   io:format("Add: ~p~n", [NewIntention]),
		       Agent#agentRationale{
			 intentions = 
			 lists:reverse([NewIntention|
					lists:reverse(Intentions)])};	   
		   #event{type = add_ejason_private_query} ->
			Agent#agentRationale{ events = 
					      [Change|Events]};


		   #event{type = ?ADDBELIEF, body = Body} ->
		 %      io:format("Body: ~p~n",[Body]),
		      Belief = utils:vars_to_bindings(Body),
		     % io:format("Adding Belief: ~p from \nBody: ~p~n",
%				[Belief,Body]),
		       case belief_base:assert(Belief,BB) of
			   {ok,no_change}->
			       Agent; %% No change
			   {ok,NewBB}->
			    %   io:format("NewBB: ~p~n",[NewBB]),
			%       {Name,Args,Annot} = Belief,
			       NewEvent =  #event{type=?ADDBELIEF, 
						  body = Belief},
			       Agent#agentRationale{belief_base = NewBB,
						   events = 
						   [NewEvent|Events]}
		       end;


		   #event{type = ?REMOVEBELIEF, body = Body} ->
		      Belief = utils:vars_to_bindings(Body),

		       case belief_base:negate(Belief,BB) of
			   {ok, no_change}->
			       Agent;
			   {ok, NewBB} ->
	
			       NewEvent =  #event{type=?REMOVEBELIEF, 
						  body = Belief},
			       Agent#agentRationale{belief_base = NewBB,
						    events = 
						    [NewEvent|Events]}
		       end;
		   #event{type = ?REMOVEADDBELIEF, body = Body} ->
		       Belief = utils:vars_to_bindings(Body),

		% io:format("Belief: ~p~nBB: ~p~n",[Belief,BB]),
		       BB2 = belief_base:negate_matching(Belief,BB),
		       
		       NewBelief = utils:vars_to_bindings(Belief),
		      % io:format("Adding Belief: ~p from \nOldBelief: ~p~n",
%				[NewBelief,Belief]),

		       case belief_base:assert(NewBelief,BB2) of
			   {ok,no_change}->
			       Agent; %% No change
			   {ok,NewBB}->
			      % io:format("NewBB: ~p~n",[NewBB]),
			       NewEvent =  #event{type=?ADDBELIEF, 
						  body = NewBelief},
			       Agent#agentRationale{belief_base = NewBB,
						    events = 
						    [NewEvent|Events]}
		       end;
		   _ when is_record(Change,event)->
		   %#event{type = ?ADDACHGOAL}->       
		       Agent#agentRationale{events = [Change|Events]};

		   ?NOEVENT -> %% This may happen when no failure event can be
		               %% generated after calling an external action
		       Agent;
		   
		   {suspend,Intention, ID}->
		       Agent#agentRationale{
			 suspended= 
			 orddict:store(ID,Intention,Suspended)};
		   {?ACTIONSUSPEND, ID,SuspendedAction, Intention}->
		       Agent#agentRationale{
			 suspended= 
			 orddict:store(ID,{SuspendedAction,Intention},
				       Suspended)};
		   
		   Other ->
		       io:format("[RC] Wrong change: ~p~n",[Other]),
		       Agent  % Change ignored
	%	   {A,B}->
	%	       io:format("A:~p~nB: ~p~n",[A,B])
	
	       end,
%    io:format("Next Changes: ~p~n",[Changes]),
    applyChanges(NewAgent,Changes);
applyChanges(Agent,Changes) ->
    io:format("[ReasoningCycle:applyChanges,error]: Invalid Agent: ~p or"++
	      "~nChanges list: ~p~n",[Agent,Changes]),
    exit(error).
					 

%wrap(_,[])->
%    [];
%wrap(Element,List)->
%    F = fun (X) -> {Element,X} end,
%    lists:map(F,List).
		   
			

selectEvent([])->
    {?NULL,?NULL};
selectEvent([Event|Events]) ->
    {Event,Events}.
 


%% Default option selection functions.
%% It returns the first applicable plan computed.
selectPlan([])->
    [];
selectPlan(ItPlans) ->
    case iterator:first(ItPlans) of
	false ->
	    [];
	Plan -> 
	  %  io:format("PLAN SELECT: ~p~n",[Plan]),
	    Plan
    end.
	    

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
selectIntention([Intention|Intentions]) ->
     {Intention,Intentions}.

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


% Only one message is processed in each iteration.
% Specified in Jason Book

gatherOneMessage() -> 
    receive 
	{signal, _Signal} -> 
%	    processSignal(Signal),
	    gatherOneMessage()
    
    after 0 ->
	    receive Message  ->
		    Message
	    after 
		0->
		    [] 
	    end
    end.

%processSignal(_Signal) ->
%    ok.

%processMessage(Agent,BB,List)->
%    processMessages(Agent,BB,List,[]).



%processMessages(_Agent,_BB,[],Acc)->
%    lists:reverse(Acc);

%% Returns the modified agent after processing the message
processMessage(Agent = #agentRationale{events = Events,
				       agentName = Name,
				       suspended = Suspended},Message)->
    %% case Message of
    %% 	[]->
    %% 	    ok;
    %% 	_ ->
    %% 	    io:format("\n[~p] at ~p receives Message: ~p~n",[Name,
    %% 				         erlang:now(),Message])
    %% end,

    NewAgent = 
	case Message of

	    % For monitored agents
	    {'DOWN', _Ref,process, DeadProc, Reason}-> 
		%		io:format("DOWNMESSAGE: ~p~n",[Message]),
		NewReason =
		    case Reason of
			noproc ->
			    {unknown_agent,{},[]};
			noconnection ->
			    {unreachable_agent,{},[]};
			_ ->
			    {dead_agent,{},[]}
		    end,
				
		NewEvent = 
		    case DeadProc of 
%			_ when is_pid(DeadProc) ->
%			    utils:add_belief([],
%					     {agent_down,{DeadProc},
%					      [{reason,{NewReason},[]}]});
			{RegName,Node} ->
			    utils:add_belief([],
					     {agent_down,
					      {{RegName,{},
						[{container,{Node},[]}]}},
					      [{reason,{NewReason},[]}]})
		    end,
		%exit(avalor),
		applyChanges(Agent,[NewEvent]);  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	     {?DM, Response= #response{id = ID}}-> 
		    
		
		case  orddict:find({?DM,ID}, Suspended) of
		    {ok, SuspendedInfo} ->
			NewSuspended = orddict:erase({?DM,ID},
						     Suspended),
			
%		    io:format("SuspendedInfo: ~p~n",[SuspendedInfo]),
			process_dm_response(Agent#agentRationale{
					      suspended = NewSuspended},
					    Response,SuspendedInfo);
		    error-> %The suspended intention does not exist
		    
			%% %io:format("No suspended action with id: ~p~n",
			%% 	      [{FindAgentID,FindAgentID}]),
			Agent
		end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	     {?SM, Response}-> 
	     	    process_sm_response(Agent,Response); 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	    {external_action,ID,Result}->
						%io:format("Match: ~p~n",[lists:keytake(ID,1,Suspended)]),


		%% {ID,RelatedIntention},
		%% NewSuspended} = 

		case
		    orddict:find(ID,Suspended) of
		    {ok,RelatedIntention} ->
			NewSuspended = 
			    orddict:erase(ID,Suspended),

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
		    
		    error ->
			io:format("[ReasoningCycle:processMessage,error]"++
				  "No suspended action with ID: ~p~n"++
				  "Suspended: ~p~n",[ID,Suspended]),
			exit(error)		 
		end;



%%%%%%%%COMMUNICATION MESSAGES (tell, untell, achieve, askone....)
	     	    
	    {communication, Sender,_Arch,{tell,Content}}->
	%	io:format("Message: ~p~n",[Message]),
		NewAnnot =
		    [{source,
		      {{Sender,{},[]}},

		      %{{Sender,{},[{container,{Arch},[]}]}},
		      % NO LONGER "CONTAINER" INCLUDED IN NAME
		      []}],
%		io:format("NewAnnot: ~p~n",[NewAnnot]),
		AnnotContent =
		    case Content of 
			A when is_atom(Content) ->
			    {A,{},[NewAnnot]};
			{Atom,Terms,Annot} ->
			    {Atom,Terms,NewAnnot++Annot}
		    end,
			
		%EventBody = utils:add_belief(Module,AnnotContent),
		NewEvent = utils:add_belief([],AnnotContent),
	%	io:format("NEWEVENT: ~p~n",[NewEvent]),
		applyChanges(Agent,[NewEvent]);
%		io:format("NA: ~p~n",[NA]),
		
	    
	    {communication, Sender, Arch, {achieve,Content}}->
		
		NewAnnot =
		    [{source,
		      {{Sender,{},[]}},
		      %{{Sender,{},[{container,{Arch},[]}]}},
		      % NO LONGER "CONTAINER" INCLUDED IN NAME

		      []}],
		AnnotContent =
		    case Content of 
			A when is_atom(Content) ->
			    {A,{},[NewAnnot]};
			{Atom,Terms,Annot} ->
			    {Atom,Terms,NewAnnot++Annot}
		    end,
		NewEvent = utils:add_achievement_goal([],AnnotContent),
		%io:format("New event: ~p~n",[NewEvent]),
		applyChanges(Agent,[NewEvent]);
	    
	    {communication, _Sender, _Arch, {untell,Content}}->
%		NewAnnot =
%		    {source,{Sender,Arch},[]},

		AnnotContent =
		    case Content of 
			A when is_atom(Content) ->
			    {A,{},[]};
			{Atom,Terms,Annot} ->
			    {Atom,Terms,Annot}
		    end,
		NewEvent = utils:remove_belief([],AnnotContent),
		applyChanges(Agent,[NewEvent]);

	    {terminate,kill} ->
		Agent#agentRationale{events = [Message|Events]};
	    {terminate,kill,_TestPid} ->
		Agent#agentRationale{events = [Message|Events]};
	    [] ->
		Agent
	end,
   
    NewAgent.

%    processMessages(Agent,BB,List,[NewEvent|Acc]).



     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     %%
     %%  Processing Messages from the Distribution Manager
     %%
     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% PARAMS: Response that reactivates the suspended action
%%         {SuspendedSendRecord,SuspendedIntention}            
%% Note: when successful, a furtherinstantiateplan + applychanges is executed
%%       otherwise, findNextFailureEvent + applyChanges
process_dm_response(Agent,    %%%%%%%%%%%%% SUSPENDED SEND
		    Response = #response{type = ?FINDAGENT},
		    {#suspendedSend{ my_name = MyName,
				     performative = Performative,
				     message = Message},
		     SuspendedIntention})->
			   
%   io:format("Find Response: ~p~n",[Response]),
    {_ID,Info,Result} = ?DM:process_response(Response),
   
    case Result of 
	?NOAGENT ->
	    FEvent = 
		findNextFailureEvent(
		  #event{relatedIntention = SuspendedIntention}),
	    NewAgent = applyChanges(
	      Agent,
	      [FEvent]),
%	    io:format("Agent after suspendedsend: ~p~n",
%		      [NewAgent]),
	    NewAgent; % SHOULD TRY A CONNECTION FIRST!
%	    Agent#agentRationale{suspended = NewSuspended};

	?AGENTFOUND ->
	    {AgentName, Container, _Pid} = Info,
%	    io:format("SuspendedIntention: ~p~n",
%		      SuspendedIntention),
	    {AgentName,Container} ! % finally send the message
		{communication,MyName,node(),
		 {Performative,Message}},
	    Change = furtherInstantiatePlan(SuspendedIntention),
%	    io:format("Change: ~p~n",[Change]),
	    NewAgent = applyChanges(Agent,
			 Change),
%	     io:format("Agent after suspendedsend: ~p~n",
%		      [NewAgent]),
	    NewAgent
    end;

process_dm_response(Agent,    %%%%%%%%%%%%% SUSPENDED CONNECT
		    Response = #response{type = ?CONNECT},
		    {#suspendedConnect{ container = Container},
		     SuspendedIntention})->
    
	   %io:format("Connect Response: ~p~n",[Response]),
    {_ID,_Info,Result} =
	?DM:process_response(Response),
    case Result of 
	?CONNECTED ->
	    Change = furtherInstantiatePlan(SuspendedIntention),
%	    io:format("Change: ~p~n",[Change]),
	    NewAgent = applyChanges(Agent,
				    Change),
%	     io:format("Agent after suspendedsend: ~p~n",
%		      [NewAgent]),
	    NewAgent;
	?NAMECLASH -> % no merge possible
	    FEvent = 
		findNextFailureEvent(
		  #event{relatedIntention = SuspendedIntention}),
	    NewAgent = applyChanges(
	      Agent,
	      [FEvent]),
%	    io:format("Agent after suspendedsend: ~p~n",
%		      [NewAgent]),
	    NewAgent
    end;
process_dm_response(Agent,    %%%%%%%%%%%%% SUSPENDED DISCONNECT
		    Response = #response{type = ?DISCONNECT},
		    {#suspendedDisconnect{ container = Container},
		     SuspendedIntention})->
    {_ID,_Info,Result} =
	?DM:process_response(Response),
    case Result of 
	?DISCONNECTED ->
	    Change = furtherInstantiatePlan(SuspendedIntention),
%	    io:format("Change: ~p~n",[Change]),
	    NewAgent = applyChanges(Agent,
				    Change),
%	     io:format("Agent after suspendedsend: ~p~n",
%		      [NewAgent]),
	    NewAgent;
	_ -> %Errors in disconnect??
	    FEvent = 
		findNextFailureEvent(
		  #event{relatedIntention = SuspendedIntention}),
	    NewAgent = applyChanges(
			 Agent,
			 [FEvent]),
%	    io:format("Agent after suspendedsend: ~p~n",
%		      [NewAgent]),
	    NewAgent
    end;
process_dm_response(Agent,    %%%%%%%%%%%%% SUSPENDED CREATEAGENT
		    Response = #response{type = ?CREATEAGENT},
		    {#suspendedCreateAgent{ agent_name = AgentName},
		     SuspendedIntention})->
    {_ID,_Info,Result} =
	?DM:process_response(Response),
    case Result of 
	?CREATED ->
	    io:format("[DEBUG:] Agent ~p created.~n",[AgentName]),

	    Change = furtherInstantiatePlan(SuspendedIntention),
%	    io:format("Change: ~p~n",[Change]),
	    NewAgent = applyChanges(Agent,
				    Change),
%	     io:format("Agent after suspendedsend: ~p~n",
%		      [NewAgent]),
	    NewAgent;
	?NAMECLASH -> 
	    io:format("[DEBUG:] Agent ~p not created.~n",[AgentName]),

	    FEvent = 
		findNextFailureEvent(
		  #event{relatedIntention = SuspendedIntention}),
	    NewAgent = applyChanges(
			 Agent,
			 [FEvent]),
%	    io:format("Agent after suspendedsend: ~p~n",
%		      [NewAgent]),
	    NewAgent
    end;

process_dm_response(Agent,    %%%%%%%%%%%%% SUSPENDED KILLAGENT
		    Response = #response{type = ?FINDAGENT},
		    {#suspendedKillAgent{},
		     SuspendedIntention})->
    {_ID,Info,Result} =
	?DM:process_response(Response),

  case Result of 
	?NOAGENT ->
	  ok;
	?AGENTFOUND ->
	    {_AgentName, _Container, Pid} = Info,
	  exit(Pid,kill)
  end,

    Change = furtherInstantiatePlan(SuspendedIntention),
%	    io:format("Change: ~p~n",[Change]),
    NewAgent = applyChanges(Agent, Change),
%	     io:format("Agent after suspendedsend: ~p~n",
%		      [NewAgent]),
    NewAgent;   

process_dm_response(Agent, Response ,
		    {SuspendedAction,
		     _SuspendedIntention})->
    io:format("[RC] Wrong DM Response with Params: ~nAgent:~p~n"++
	      "Response: ~p~n",
	      "SuspendedAction: ~p~n",
	      [Agent,Response,SuspendedAction]),
    Agent.


%% {?FAIL}-> % attempt connection
%%     case ?DM:connect(Container) of
%% 	{?STUTTERACTION}->
%% 	    case ?DM:find_agent(AgentName) of % Search agai
%% 		{AgentName,
%% 		 AgentContainer,_Pid} ->
%% 		    {AgentName,AgentContainer} !
%% 			{communication,MyName,node(),
%% 			 {NewIntention,Message}},
%% 		    {?STUTTERACTION};
%% 		_ ->
%% 		    {?FAIL}
%% 	    end;






     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     %%
     %%  Processing Messages from the Supervision Manager
     %%
     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_sm_response(_,_)->
    ?STUTTERACTION.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  PLAN FAILURE HANDLING
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Generates the failure event corresponding to some event
% By now, only for failed goal addition plans.
make_failure_event(Event = #event{type=Type,
				  body = Body})->
%io:format("MakeFailEvent: Event: ~p~n",[Event]),
    case Type of
	?FAILEDACHGOAL ->
	    Event;
	?FAILEDTESTGOAL->
	    Event;
	?ADDACHGOAL ->
	    Event#event{type= ?FAILEDACHGOAL,
			body =setelement(2,Body,?FAILEDACHGOAL)};
	?ADDTESTGOAL ->
	    Event#event{type= ?FAILEDTESTGOAL,
			body =setelement(2,Body,?FAILEDTESTGOAL)};
	_->
	    findNextFailureEvent(Event)

%        add_ejason_private_query ->
	    
%	_Other ->
%	    io:format("[reasoningCycle.erl] Warning: trying to get"++
%		      " failure plan for an event of type ~p~n.",[Type]),
%	    ?NOEVENT
    end.


%% Identifies the intended means for the chosen event
%% It changes the event if a failure plan is detected.
findIntendedMeans(_NewAgent,Event = [],_Plans,_OptionSelector) ->
    {Event,[]};
findIntendedMeans(NewAgent,Event,Plans,OptionSelector) ->
    IntendedMeans =
	case Event of 
	    {terminate,kill}->
		{terminate,kill};
	    {terminate,kill,Test}->
		{terminate,kill,Test};
	    _ ->
		
		ItApplicablePlans =
		    findApplicablePlans(NewAgent,Event,Plans),
		%io:format("~n~nALL APPLICAPLANS: ~p~n~n~n",
		%	  [iterator:get_all(ItApplicablePlans)]),
		OptionSelector(ItApplicablePlans)
	end,
    case IntendedMeans of
	[]->
	    FailureEvent = make_failure_event(Event),
%	   io:format("FailureEvent: ~p~n",[FailureEvent]),
	  %  exit(err),
	    %% If IntendedMeans is empty, look for a failure plan
	    findFailureIntendedMeans(NewAgent,FailureEvent,
				     Plans,OptionSelector);
	_ ->% Intended Means (plan) found. Event remains unchanged
	    {Event,IntendedMeans}
    end.


%% Looks for the intended means for a failure event given as input.
%% It not found, generates failure events for the intention stack
%% and iterates. 
findFailureIntendedMeans(_Agent,?NOEVENT,_Plans,_OptionSelector)->
    %io:format("Drop Intention: ~p~n",[DropIntention]),
    % No failure plan found
%    DropIntention;
    {?NOEVENT,[]};
findFailureIntendedMeans(Agent,FailureEvent,Plans,OptionSelector)->
%    FailureEvent = make_failure_event(Event),
 %  io:format("FEvent: ~p~n",[FailureEvent]),
    ItFailureApplicablePlans = 
	findApplicablePlans(Agent,
			    FailureEvent,Plans),
    case OptionSelector(ItFailureApplicablePlans) of
	[] ->
    % If the Event has some related intention, the plan on top of it
    % fails.
	    NewFailureEvent=findNextFailureEvent(FailureEvent),
 
	    findFailureIntendedMeans(Agent,NewFailureEvent,
				     Plans,OptionSelector);
	%	    end;		 
	PlanforIntendedMeans ->
%	    io:format("PlanForFailure: ~p~n",[PlanforIntendedMeans]),
	    {FailureEvent, PlanforIntendedMeans}
    end.



% Generates the proper failure event from the intention stack
% Only plans for goal addition may have failure plans
findNextFailureEvent(_Event =  #event{relatedIntention = []})->
    %% No upper-level failure plan can be generated. 
    %%The intention will be dropped
    ?NOEVENT;
findNextFailureEvent(_Event = #event{relatedIntention =
				    RelatedIntention})->
%       io:format("Related: ~p~n",[RelatedIntention]),

 [_PiPlan = #piPlan{init_bindings = 
		      {[VarName,VarType|InitBindings],
		      InitAnnotations}
		     }|Rest] = RelatedIntention,
 %   io:format("PiPlan: ~p~n",[PiPlan]),
    
    case VarType#var.bind of
	?ADDACHGOAL -> %only plans for achievement goals can fail
	    FEvent = #event{ type = ?FAILEDACHGOAL,
			     body = {VarName,?FAILEDACHGOAL,
					 list_to_tuple(InitBindings),
				     InitAnnotations},
			     relatedIntention = Rest},
	%	    io:format("Next Failure Event: ~p~n",[FEvent]),
	    FEvent;
	?ADDTESTGOAL -> %only plans for achievement goals can fail
	    FEvent = #event{ type = ?FAILEDTESTGOAL,
			     body = {VarName,?FAILEDTESTGOAL,
				     list_to_tuple(InitBindings),
				     InitAnnotations},
			     relatedIntention = Rest},
	%	    io:format("Next Failure Event: ~p~n",[FEvent]),
	    FEvent;
	_ -> % cannot fail, try with next
	    findNextFailureEvent(#event{relatedIntention = Rest})
    end.

