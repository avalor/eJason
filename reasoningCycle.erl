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

-compile(export_all).

-define(NULL, []).
-include("macros.hrl").


start(AgentName,InitialEvents, Plans,BB)->
    #agentRationale{agentName = AgentName,
		    plans = Plans,
		    belief_base = BB,
		    events = InitialEvents}.

reasoningCycle(#agentRationale{ events = [],
			       intentions = []}= Agent)->
   % io:format("[~p] Nothing to do. 5 sec. sleep.~n",[Agent#agentRationale.agentName]),
    timer:sleep(200), %% Comment to skip process sleep time
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
    
 %  io:format("INTENDED MEANS: ~p~n",[IntendedMeans]),
    AllIntentions = processIntendedMeans(Event,Intentions,IntendedMeans),
%   io:format("All Intentions: ~p~n",[AllIntentions]),
    
    case IntentionSelector(AllIntentions) of
	{{terminate,kill},_}->
	    io:format("Agent ~p killed.~n",[Agent#agentRationale.agentName]);
	{{terminate,kill,TestPid},_} when is_pid(TestPid)->
	    TestPid ! {killed, #agentRationale.agentName};
	    %io:format("Agent ~p killed. Answer sent to ~p~n",
	%	      [Agent#agentRationale.agentName,TestPid]);
	[] ->
	    NewAgent = 
		check_mailbox( Agent#agentRationale{
				 events = NotChosenEvents}),
	    reasoningCycle(NewAgent);
	
	{Intention,NotChosenIntentions} ->
%	    io:format("Chosen Intention: ~p~n",[Intention]),
	    
	    Result = executeIntention(BB,Intention),
						%io:format("RESTART~n"),
	    %io:format("Result from Execute intention is ~p~n",[Result]),
	    NewAgent = 
		applyChanges(Agent#agentRationale
			     {events = NotChosenEvents,
			      intentions = NotChosenIntentions},
				     Result),
	    %io:format("NewAgent: ~p~n",[NewAgent]),
	   reasoningCycle(check_mailbox(NewAgent));
	Other->
	    io:format("ERROR: Intention would be: ~p~n",[Other])
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



%% Intention = [{Event,Plan,Valuation,[{FormulaFunction,Bindings}] }
%% Last event and plan are included to enhance observational power of
%% the intention selection function.

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
		     {#plan{body=FormulaList}=Plan,Valuation}) ->
    
%    io:format("EventType: ~p~n",[Event#event.type]),
    NewIntention =  
	case Event#event.type of
	    external->
		[#siPlan{ event = Event,
			 plan = Plan,
			 valuation = Valuation,
			 formulas = FormulaList}];
			 % IM create a new intention
	internal->
		RIntention = Event#event.relatedIntention,
		NewSIPlan = #siPlan{ event = Event#event{relatedIntention = undefined},
				     plan = Plan,
				     valuation = Valuation,
				     formulas = FormulaList},
		[NewSIPlan|RIntention]
	end,
    [NewIntention|Intentions].



%% Executes the intention chosen
%% Returns a list of state updates (e.g. adding new events or updating intention list)
executeIntention(_BBID,[])->
    io:format("No intention executable");
executeIntention(BBID,Intention = [SIPlan|_RestSIP])->
%    io:format("Executing Intention: ~p~n",[Intention]),
   
    Formula = case SIPlan#siPlan.formulas of
		  [{NewFormula,_Bindings}|_RestFormulas]->
		      NewFormula;
		  [TestGoalBodyFormula] ->
		      TestGoalBodyFormula
	      end,
    Valuation = SIPlan#siPlan.valuation,
    AnswerList = Formula(BBID,Valuation),
    processAnswers(Intention, AnswerList,[]).


%% Updates the valuation of the SIP with the new bindings in the obtained valuations
%% Removes the last executed formula from the body of the SIP and removes the SIP
%% after the execution of the last formula.
furtherInstantiatePlan([],_ObtainedValuation)->  
    [{deleteIntention,[]}];
furtherInstantiatePlan([SIPlan|RestSIP],ObtainedValuation)->
%    io:format("Instantiate with valuation: ~p~n",[ObtainedValuation]),
   {[NewValuation],RestFormulas} = 
	case SIPlan#siPlan.formulas of
	    [{_UsedFormula,Bindings}|NextFormulas] ->
		OldValuation = SIPlan#siPlan.valuation, 
		    case ObtainedValuation of
			[]->
			    {[OldValuation],NextFormulas};
			_ ->
			    {utils:updateValuation(
			      [OldValuation], ObtainedValuation,Bindings),
			     NextFormulas}
		    end;
	    
	    [TestGoalFormula] when is_function(TestGoalFormula)->
		{[ObtainedValuation],[]}
	end,
    

 %   io:format("RestFormulas: ~p~n",[RestFormulas]),
    case RestFormulas of 
	[] -> %% Test Goal just executed
    	    furtherInstantiatePlan(RestSIP,NewValuation);
	[LastFormula] ->
%	    io:format("LastFormula: ~p~n",[LastFormula]),
	    [{finished,OtherValuation}] = LastFormula(NewValuation),
	    furtherInstantiatePlan(RestSIP,OtherValuation);
	RestFormulas when length(RestFormulas) >1 ->
		    NewSIP = SIPlan#siPlan{valuation = NewValuation,
					  formulas = RestFormulas},
		    [{addIntention,[NewSIP|RestSIP]}]
    end.


processAnswers(_IntentionExecuted,[],Acc)->
    Res = lists:reverse(Acc),
%    io:format("Changes after Answers: ~p~n",[Res]),
    Res;
processAnswers(Intention,[Answer|Answers],Acc)->
  %io:format("Processing ANSWER: ~p~n",[Answer]),
    
	 
    Result = case Answer of
		 {finished, NewValuation}->% SIP completely executed
		     furtherInstantiatePlan(Intention,NewValuation);

		 {add_achievement_goal, _NewEvent} ->
		     [{addEvent, #event{type=internal, body = Answer,
					relatedIntention=Intention}}];%++
%			 furtherInstantiatePlan(Intention,[]);

		 {add_test_goal, _NewEvent} ->
		     [{addEvent, #event{type=internal, body = Answer,
				       relatedIntention=Intention}}];%++
%			 furtherInstantiatePlan(Intention,[]);

		 {stutter_action} ->
		 	 furtherInstantiatePlan(Intention,[]);

		 {add_belief, _Belief}->
		     [Answer|furtherInstantiatePlan(Intention,[])];
		 {remove_belief, _Belief}->
		     [Answer|furtherInstantiatePlan(Intention,[])];
		 {remove_add_belief, _Belief}->
		     [Answer|furtherInstantiatePlan(Intention,[])];
		 {new_intention_goal, NewEvent} ->
		     [{addEvent, #event{type=external, 
					body = 
					{add_achievement_goal,NewEvent}}}]++
		      furtherInstantiatePlan(Intention,[]);

		 _ ->
		     io:format("Unknown Answer: ~p~n",[Answer]),
		     []
	     end,
    
    case Result of
	[]->
	    processAnswers(Intention,Answers,Acc);
	_ ->
	    processAnswers(Intention,
			   Answers,lists:append(Result,Acc))
    end.




applyChanges(Agent,[]) ->
%   io:format("Final Agent: ~p~n",[Agent]),
   Agent;
applyChanges(#agentRationale{events = Events,
			     intentions = Intentions,
			    belief_base = BB}=Agent,
	     [Change|Changes])->
% io:format("Change: ~p~nWith BB:~p~n",[Change,BB]),
    NewAgent = case Change of
		   {deleteIntention,_}->
		       Agent;
		   {addIntention,NewIntention} ->
		       Agent#agentRationale{intentions = [NewIntention|Intentions]};
		   {addEvent, NewEvent} ->
		       Agent#agentRationale{events = [NewEvent|Events]};
		   {add_belief, Belief} ->
		  %     io:format("Adding Belief: ~p~n",[Belief]),
		       case beliefbase:assert(Belief,BB) of
			   {ok,no_change}->
			       Agent; %% No change
			   {ok,NewBB}->
			       NewEvent =  #event{type=internal, 
						  body = Change},
			       Agent#agentRationale{belief_base = NewBB,
						   events = 
						   [NewEvent|Events]}
		       end;
		   {remove_belief, Belief} ->
		       case beliefbase:deny(Belief,BB) of
			   {ok, no_change}->
			       Agent;
			   {ok, NewBB} ->
			       NewEvent =  #event{type=internal, 
						  body = Change},
				   Agent#agentRationale{belief_base = NewBB,
							events = 
							[NewEvent|Events]}
		       end;
		  {remove_add_belief, Belief} ->
		       BB2 = beliefbase:deny_matching(Belief,BB),
		       case  beliefbase:assert(Belief,BB2) of
			   {ok,no_change} ->
			       Agent#agentRationale{belief_base = BB2};
			   {ok,NewBB} ->
			       NewEvent =  #event{type=internal, 
						  body = {add_belief,Belief}},
			       Agent#agentRationale{belief_base = NewBB,
						    events = 
						    [NewEvent|Events]}
		       end;
			   
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
	







