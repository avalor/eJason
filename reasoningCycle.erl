-module(reasoningCycle).

-compile(export_all).

-define(NULL, []).

-include("macros.hrl").

%% -record(agentRationale, {events=[], agentName="", 
%% 			 intentions = [],
%% 			 plans = [],
%% 			 belief_base = undefined,
%% 			 eventSelector = fun reasoningCycle:selectEvent/1, 
%% 			 optionSelector = fun reasoningCycle:selectPlan/1, 
%% 			 intentionSelector = fun reasoningCycle:selectIntention/1}).

%% -record(event, {type = external, body = {}, relatedIntention=undefined}).
%% -record(plan,{trigger=fun(_X)-> false end,context, body}). %A function each


start(AgentName,InitialEvents, Plans,BB)->
    #agentRationale{agentName = AgentName,
		    plans = Plans,
		    belief_base = BB,
		    events = InitialEvents}.

reasoningCycle(#agentRationale{agentName = Name,
			       events = [],
			       intentions = []}= Agent)->
   % io:format("[~p] Nothing to do. 5 sec. sleep.~n",[Name]),
   % timer:sleep(10),
    NewAgent = check_mailbox(Agent),
    reasoningCycle(NewAgent);
reasoningCycle(#agentRationale{eventSelector = EvSel, 
			       events = Events,
			       belief_base = BB,
			       agentName = _AgentName,
			       plans = Plans,
			       intentions = Intentions,
			       optionSelector = OptionSelector,
			       intentionSelector = IntentionSelector}= Agent)->

%    io:format("Agent: ~p~n",[Agent]),
    

    {Event, NotChosenEvents} = EvSel(Events),
    IntendedMeans = 
	case Event of 
	    []-> 
		[];
	    {terminate,kill}->
		{terminate,kill};
	    {terminate,kill,Test}->
		{terminate,kill,Test};
	    _ ->
%		io:format("CHOSEN EVENT: ~p~n",[Event]),
		RelevantPlans = findRelevantPlans(Event,Plans),
%		io:format("RELEVANT PLANS: ~p~n",[RelevantPlans]),
		ApplicablePlans = unifyContext(BB, RelevantPlans),
%		io:format("Applicable PLANS: ~p~n",[ApplicablePlans]),
		OptionSelector(ApplicablePlans)
	      end,
    
%    io:format("INTENDED MEANS: ~p~n",[IntendedMeans]),
    AllIntentions = processIntendedMeans(BB,Event,Intentions,IntendedMeans),
 %   io:format("All Intentions: ~p~n",[AllIntentions]),
    
    case IntentionSelector(AllIntentions) of
	{{terminate,kill},_}->
	    eresye:stop(BB),
	    io:format("Agent ~p killed.~n",[Agent#agentRationale.agentName]);
	{{terminate,kill,TestPid},_} when is_pid(TestPid)->
	    eresye:stop(BB),
	    TestPid ! {killed, #agentRationale.agentName};
	    %io:format("Agent ~p killed. Answer sent to ~p~n",
	%	      [Agent#agentRationale.agentName,TestPid]);
	[] ->
	    NewAgent = 
		check_mailbox( Agent#agentRationale{
				 events = NotChosenEvents}),
	    reasoningCycle(NewAgent);
	
	{Intention,NotChosenIntentions} ->
						%io:format("RESTART~n"),
	    
	    Result = executeIntention(Intention),
						%io:format("RESTART~n"),
	    %io:format("Result from Execute intention is ~p~n",[Result]),
	    NewAgent = 
		applyChanges(Agent#agentRationale
			     {events = NotChosenEvents,
			      intentions = NotChosenIntentions},
				     Result),
	  % io:format("NewAgent: ~p~n",[NewAgent]),
	   reasoningCycle(check_mailbox(NewAgent))
    end.



findRelevantPlans(?NULL,_Plans)->
    [];
findRelevantPlans(Event,Plans)->
    findRelevantPlans(Event,Plans,[]).


%% Implementation for findRelevantPlans/3 with tail-recursion
findRelevantPlans(_, [], Acc) ->
    Acc;
findRelevantPlans(#event{body=EBody}=Event,[Plan = #plan{trigger=T}|Plans], Acc)->
  %  io:format("FindRelevantPlans with ~p~n",[T]),
    case T(EBody) of
	{true,InitVal} ->
	    findRelevantPlans(Event,Plans,[{Plan,InitVal}|Acc]);
	false ->
	    findRelevantPlans(Event,Plans,Acc)
    end.
	    
	

				      
%% Returns a list [{plan#plan{}, Valuation}]

unifyContext(BBID,RelevantPlans)->
    unifyContext(BBID,RelevantPlans, []).

unifyContext(_BBID,[],Acc)->
    Acc;
unifyContext(BBID,[{#plan{context=Context}=Plan,InitVal}|Plans], Acc) ->
   % io:format("BBID: ~p~n,InitVal: ~p~nPlan: ~p~n",[BBID,InitVal,Plan]),
    
    case Context(BBID,InitVal) of
	[] ->
	    unifyContext(BBID,Plans,Acc);
	Valuations ->
	    
	    %%Same copy of plan,for the different valuations
	   % io:format("Plan: ~p~nValuations~p~n",[Plan,Valuations]),

	    NewApplicablePlans = wrap(Plan,Valuations),	
	    %io:format("NEWAPPLICABLE: ~p~n",[NewApplicablePlans]),
	    unifyContext(BBID,Plans,Acc++NewApplicablePlans)
    end.



%% Intention = {Event,{Plan,Valuation},[Pid]}
%% Last event and plan are included to enhance observational power of
%% the intention selection function.

processIntendedMeans(_BBID,_Event,Intentions,[]) ->
    Intentions;
processIntendedMeans(_BBID,_Event,Intentions,{terminate,kill}) ->
    I = [{terminate,kill}|lists:reverse(Intentions)], 
    %%Terminate only when there are no more intentions
    lists:reverse(I);
processIntendedMeans(_BBID,_Event,Intentions,{terminate,kill,TestPid}) ->
    I = [{terminate,kill,TestPid}|lists:reverse(Intentions)], 
    %%Terminate only when there are no more intentions
    lists:reverse(I);
processIntendedMeans(BBID,Event,Intentions,{#plan{body={M,F}}=Plan,Valuation}) ->
    Pid = spawn(M,F,[BBID,Valuation]),
%    io:format("New Pid for ~p:~p ~p~n",[M,F,Pid]),
%    io:format("EventType: ~p~n",[Event#event.type]),
    case Event#event.type of
	external->
	    [{Event,{Plan,Valuation},[Pid]}|Intentions];% IM create a new intention
	internal->
	    {_,_,Pids} = Event#event.relatedIntention, %%Old event, plan and valuation overwritten
	    [{Event#event{relatedIntention = undefined},{Plan,Valuation},[Pid|Pids]}|Intentions]%IM put on top of an existing intention
    end.


%% Executed the intention chosen
%% Returns a list of state updates (e.g. adding new events or updating intention list)
executeIntention([])->
    io:format("No intention executable");
executeIntention({Event,{Plan,Valuation},[Pid|Pids]})->
    Pid ! {execute,Valuation,self()},
    receive	
	
	{AnswersList, Pid}->
	 %  io:format("Answerlist: ~p~n",[AnswersList]),
	    processAnswers({Event,{Plan,Valuation},[Pid|Pids]}, AnswersList,[])
    end.


processAnswers(_IntentionExecuted,[],Acc)->
    Res = lists:reverse(Acc),
%    io:format("Changes after Answers: ~p~n",[Res]),
    Res;
processAnswers({Event,{Plan,Valuation},[Pid|Pids]},[Answer|Answers],Acc)->
%  io:format("Processing ANSWER: ~p~n",[Answer]),

    Result = case Answer of
		 {finished, NewValuation}->
		     case Pids of
			 [] ->
			     [{deleteIntention,[]}];
			 _ ->
			  %   io:format("Pids: ~p~n",[Pids]),
			     [{addIntention,{Event,{Plan,NewValuation},Pids}}]
		     end;
		 {finished_test_goal, NewValuation}->
		     [{addIntention,{Event,{Plan,NewValuation},Pids}}];
	       
		 {achievementGoal, NewEvent} ->
		     [{addEvent, #event{type=internal, body = NewEvent,
				       relatedIntention={Event,{Plan,Valuation},
							 [Pid|Pids]}}}];
		 {testGoal, NewEvent} ->
		     [{addEvent, #event{type=internal, body = NewEvent,
				       relatedIntention={Event,{Plan,Valuation},
							 [Pid|Pids]}}}];
		 {stutter_action} ->
		     [{addIntention,{Event,{Plan,Valuation},
				    [Pid|Pids]}}];
		 {add_internal_event, NewEvent} ->
		     [{addIntention,{Event,{Plan,Valuation},
				    [Pid|Pids]}},
		     {addEvent, #event{type=internal, body = NewEvent}}];
		 {add_external_event, NewEvent} ->
		     [{addIntention,{Event,{Plan,Valuation},
				    [Pid|Pids]}},
		      {addEvent, #event{type=external, body = NewEvent}}]
	     end,
    
    case Result of
	[]->
	    processAnswers({Event,{Plan,Valuation},[Pid|Pids]},Answers,Acc);
	_ ->
	    processAnswers({Event,{Plan,Valuation},[Pid|Pids]},
			   Answers,lists:append(Result,Acc))
    end.
	



applyChanges(Agent,[]) ->
%   io:format("Final Agent: ~p~n",[Agent]),
   Agent;
applyChanges(#agentRationale{events = Events,
				  intentions = Intentions}=Agent,[Change|Changes])->
 %io:format("Change: ~p~n",[Change]),
    NewAgent = case Change of
		   {deleteIntention,_}->
		       Agent;
		   {addIntention,NewIntention} ->
		       Agent#agentRationale{intentions = [NewIntention|Intentions]};
		   {addEvent, NewEvent} ->
		       Agent#agentRationale{events = [NewEvent|Events]};
		   {none} ->
		       Agent
	       end,
    applyChanges(NewAgent,Changes).
	       
					 

wrap(_,[])->
    [];
wrap(Element,List)->
    F = fun (X) -> {Element,X} end,
    lists:map(F,List).
		   
			


selectEvent([])->
    {?NULL,?NULL};
selectEvent([Event|Events]) ->
    {Event,Events}.
 
selectPlan([])->
    [];
selectPlan([Plan|_Plans]) ->
    Plan.


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

check_mailbox(Agent=#agentRationale{events = Ev,
				   belief_base = BB})->
    Messages = gatherAllMessages(),
    NewEvents = processMessages(BB,Messages),
    NewAgent = Agent#agentRationale{events = Ev++NewEvents},
    %io:format("NewAgent: ~p~n",[NewAgent]), 
    NewAgent.


gatherAllMessages()->
    gatherAllMessages([]).

gatherAllMessages(Acc) ->
    receive
	Message ->
	  %  io:format("Message received: ~p~n",[Message]),
	    gatherAllMessages([Message|Acc])
    after 0 ->
	    Acc
    end.

processMessages(BB,List)->
    processMessages(BB,List,[]).

processMessages(_,[],Acc)->
    Acc;
processMessages(BB,[Message|List],Acc)->
    NewEvent = 
	case Message of
	    {communication, _Sender,{tell,Content}}->
		utils:add_belief(BB,Content);
	    {communication, _Sender,{achieve,Content}}->
		utils:add_achievement_goal(Content);
	    {communication, _Sender,{untell,Content}}->
		utils:remove_belief(BB,Content);
	    {terminate,kill} ->
		{terminate,kill};
	    {terminate,kill,TestPid} ->
		{terminate,kill,TestPid}
	end,
    processMessages(BB,List,[NewEvent|Acc]).
