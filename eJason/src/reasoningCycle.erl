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
-include("ejason.hrl").
-include("macros.hrl").
-include("parser.hrl").


start(AgentName,InitialEvents, Plans,BB)->
%io:format("New AgentName: ~p~n",[AgentName]),
    #agentRationale{agentName = AgentName,
		    plans = Plans,
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
   %io:format("Initil Agent: ~p~n",[Agent]),
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
		    selectIntention = IntentionSelector} =
	NewAgent,
    
    {Event, NotChosenEvents} = EvSel(Events),
%  case AgentName of
 %     owner ->
%	  io:format("~p BB: ~p~n",[AgentName,BB]);

%	  io:format("~p Selected Event: ~p~n",[AgentName,Event]),
 %     _ -> 
	  
  %{}
 %w end,
    
      

    IntendedMeans = 
	case Event of 
	    []-> 
		[];
	    {terminate,kill}->
		{terminate,kill};
	    {terminate,kill,Test}->
		{terminate,kill,Test};
	    _ ->

		ItApplicablePlans =
		    findApplicablePlans(NewAgent,Event,Plans),
		OptionSelector(ItApplicablePlans)
	end,
    
%  io:format("INTENDED MEANS: ~p~n",[IntendedMeans]),
 %   exit(avalor),

    AllIntentions = processIntendedMeans(Event,Intentions,IntendedMeans),
 %  io:format("All Intentions: ~p~n",[AllIntentions]),
    
    case IntentionSelector(AllIntentions) of
	{{terminate,kill},_}->
	    io:format("Agent ~p killed.~n",[Agent#agentRationale.agentName]);
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
	    
	    Result = executeIntention(BB,ModuleName,Intention),
%	   io:format("Result from Execute intention is ~p~n",[Result]),
	    FinalAgent = 
		applyChanges(Agent#agentRationale
			     {events = NotChosenEvents,
			      intentions = NotChosenIntentions},
				     Result),
%	   io:format("FinalAgent: ~p~n",[FinalAgent]),
	   reasoningCycle(FinalAgent#agentRationale{
			    info={0,1,?MBOXNOTCHECKED}});%% Info is RESTARTED
	Other->
	    io:format("ERROR: Intention would be: ~p~n",[Other])
    end.


% Generates an iterator that computes all applicable plans
findApplicablePlans(Agent = #agentRationale{belief_base = BB},
		    #event{type = Type,body=EBody},PlanList)->
% io:format("PlanList: ~p~n",[PlanList]),
%	  io:format("BB: ~p~nType: ~p~nBody: ~p~nPlan: ~p~n",
 %   [BB,Type,EBody,PlanList]),

    ItPlans = iterator:create_iterator(PlanList),
    case Type of
	add_ejason_private_query ->
	    case private_query:resolve_query(Agent,EBody) of
		{NewBindings,RetParams}->
		    iterator:create_iterator(
		      [#plan{bindings = NewBindings,
			     return_params = RetParams,
			     body = [fun private_query:formula/1]}]);
		false ->
		    iterator:create_iterator([])
	    end;
	_ ->

	    Fun =  fun (Plan) -> match_plan(BB,Type,EBody,Plan) end,
	    iterator:create_iterator_fun(ItPlans,Fun)
    end.



match_plan(BB,EventType,EventBody,% = {Name,Args,_Annot},
	   Plan = #plan{trigger=Trigger,bindings=Bindings}) ->
%   io:format("EventType: ~p~nEvent: ~p~nPlan: ~p~n",
%	      [EventType,EventBody,Plan]),
 %io:format("EventBodyList: ~p\n",
%	      [tuple_to_list(EventBody)]),

    Params  = 
	case tuple_to_list(EventBody) of

%% TODO: check why these are the same
	    [Name,EventType,Args,Annot] ->% achievement goal
		[BB,Bindings,Name,EventType]++tuple_to_list(Args)++[Annot];

	    [Name,Args,Annot] -> %add/remove belief
		[BB,Bindings,Name,EventType]++tuple_to_list(Args)++[Annot];
	    [Name,EventType,ModuleName,Args,_Annot] -> % Test goal
		[BB,Bindings,Name,EventType,ModuleName,Args]
	end,

%io:format("Params: ~p~n",[Params]),
%exit(avalor),
	%case Args of
	 %   {} ->
	%	[];
	 %   _ ->
%		tuple_to_list(Args)
%	end,
%    io:format("Trigger: ~p~nParams: ~p~nLength: ~p~n",
%	      [Trigger,Params,length(Params)]),
    try
	case apply(Trigger,Params) of
	    false -> false; % The plan is not applicable
	    ItContext when is_function(ItContext) -> % The trigger matches
%	    io:format("Matched Trigger: ~p~nParams: ~p~nLength: ~p~n",
%	      [Trigger,Params,length(Params)]),
		%exit(avalor),
%		io:format("ItContext genera: ~p~n",
%			  [iterator:get_all(ItContext)]),
		ContextFun = 
		    fun ({RetParams,NewBindings}) -> %% not a test goal  
			   %io:format("AB: ~p~n",[A]),
			    
			    %NewParams = 
			%	variables:valuate_params(RetParams,NewBindings),
%			    io:format("NewParams: ~p~n",[NewParams]),

		%	    FinalBindings =
		%		variables:update(NewBindings, NewParams),
			    Plan#plan{bindings = NewBindings,
				     return_params = RetParams}
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
		io:format("[reasoningcycle]error: match_plan1(~p)~n",[Other])

	end
    catch
	error:{badarity,_Error}-> %% The plan is not applicable
%	    io:format("Error: badarity~n",[]),
	    false ;
	  A:B ->
	    io:format("[RC] Unexpected error: ~p:~p~n \n\n",[A,B]),
	    exit(avalor)
    end.


    

%findRelevantPlans(?NULL,_Plans)->
%    [];
%findRelevantPlans(Event,Plans)->
%    findRelevantPlans(Event,Plans,[]).


%% Returns an iterator that generates all relevant plans
%% derived from the same plan (different context matchings).
%findRelevantPlans(EBody,#plan{trigger=T})->
%    T(EBody).




				      
%% Returns a list [{plan#plan{}, Valuation}]

%unifyContext(BBID,RelevantPlans)->
%    unifyContext(BBID,RelevantPlans, []).
 
%unifyContext(_BBID,[],Acc)->
%    Acc;
%unifyContext(BBID,[{#plan{context=Context}=Plan,InitVal}|Plans], Acc) ->
   % io:format("BBID: ~p~n,InitVal: ~p~nPlan: ~p~n",[BBID,InitVal,Plan]),
    
%    case Context(BBID,InitVal) of
%	[] ->
%	    unifyContext(BBID,Plans,Acc);
%	Valuations ->
%	    
%	    %%Same copy of plan,for the different valuations
%	   % io:format("Plan: ~p~nValuations~p~n",[Plan,Valuations]),
%
%	    NewApplicablePlans = wrap(Plan,Valuations),	
%	    %io:format("NEWAPPLICABLE: ~p~n",[NewApplicablePlans]),
%	    unifyContext(BBID,Plans,Acc++NewApplicablePlans)
%   end.
%
%
%
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
		     #plan{body = FormulaList,
			  bindings = Bindings}=Plan) ->
    
%    io:format("EventType: ~p~n",[Event#event.type]),
    NewIntention =  
	case Event#event.relatedIntention of
	    undefined->
		[#piPlan{ event = Event,
			  plan = Plan#plan{bindings = []},
			  valuation = Bindings,
			  params = [],
			  formulas = FormulaList}];
			 % IM create a new intention
	    RIntention->
		NewSIPlan = #piPlan{ event = 
				     Event#event{relatedIntention = undefined},
				     %%Remove initial bindings
				     plan = Plan#plan{bindings = []},
				     valuation = Bindings,
				     formulas = FormulaList},
		[NewSIPlan|RIntention]
	end,
    [NewIntention|Intentions].



%% Executes the intention chosen
%% Returns a list of state updates (e.g. adding new events or updating intention list)
executeIntention(_BBID,_ModuleName,[])->
    io:format("No intention executable"),
    [];
executeIntention(_BBID,ModuleName,Intention = [SIPlan|_RestSIP])->
%    io:format("Executing Intention: ~p~n",[Intention]),
%    case ModuleName of
%      _ ->
%	    io:format("Executing Intention: ~p~n",[SIPlan]);

%      _ -> 
%	  
%	  {}
 %end, 
%Formula = case SIPlan#piPlan.formulas of
%		  [{NewFormula,_Bindings}|_RestFormulas]->
%		      NewFormula;
%		  [TestGoalBodyFormula] ->
%		      TestGoalBodyFormula
%	      end,

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


%% Updates the valuation of the SIP with the new bindings in the 
%% obtained valuations
%% Removes the last executed formula from the body of the SIP 
%% and removes the SIP after the execution of the last formula.

furtherInstantiatePlan([])->  
    [{deleteIntention,[]}];
furtherInstantiatePlan([PIP = #piPlan{formulas = Formulas,
			       valuation = Bindings,
			       plan = #plan{return_params = Params}}|RestSIP])->
    %io:format("Instantiate with valuation: ~p~n",[ObtainedValuation]),
     %  {[NewValuation],RestFormulas} = 
  %  io:format("Formulas: ~p~n",[Formulas]),
    NewIntention = 
	case Formulas of
	    [] ->
%		io:format("RestSip: ~p~n",[RestSIP]),

		case RestSIP of 
		    [] -> % No more plans in the intention
			[{deleteIntention,[]}]; 
		    [NextPIP = #piPlan{formulas = _NextFormulas,
				       valuation = OldBindings}|Rest] ->
	%	      	io:format("Params: ~p~nBindings: ~p~n",[Params,Bindings]),
			NewParams = 
			    variables:valuate_params(Params,Bindings),
	%		io:format("NewParams: ~p~n",[NewParams]),
			NewValuation = 
			    variables:update(OldBindings,NewParams),
	%	        io:format("NewValuation: ~p~n",[NewValuation]),
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
		[PIP#piPlan{formulas = NextFormulas}|RestSIP]
	end,

%    io:format("NewIntention: ~p~n",[NewIntention]),

   case NewIntention of 
       [{_,[]}] ->
	  % io:format("NewIntention at last: ~p~n",[NewIntention]),

	    [{deleteIntention,[]}];
       _ ->
	  
	   [{addIntention,NewIntention}]
   end.
		       
%		OldValuation = SIPlan#piPlan.valuation, 
%		    case ObtainedValuation of
%			[]->
%			    {[OldValuation],NextFormulas};
%%			_ ->
%			    {utils:updateValuation(
%			      [OldValuation], ObtainedValuation,Bindings),
%			     NextFormulas}
%		    end;
	    
%	    [TestGoalFormula] when is_function(TestGoalFormula)->
%		{[ObtainedValuation],[]}
%	end,
    

 %   io:format("RestFormulas: ~p~n",[RestFormulas]),
 %   case RestFormulas of 
%	[] -> %% Test Goal just executed
 %   	    furtherInstantiatePlan(RestSIP,NewValuation);
%	[LastFormula] ->
%	    io:format("LastFormula: ~p~n",[LastFormula]),
%	    [{finished,OtherValuation}] = LastFormula(NewValuation),
%	    furtherInstantiatePlan(RestSIP,OtherValuation);
%	RestFormulas when length(RestFormulas) >1 ->
%		    NewSIP = SIPlan#piPlan{valuation = NewValuation,
%					  formulas = RestFormulas},
%		    [{addIntention,[NewSIP|RestSIP]}]
%    end.


%processAnswer(IntentionExecuted,Answers)->
%    processAnswer(IntentionExecuted,Answers,[]).


%processAnswer(_IntentionExecuted,[],Acc)->
%    Res = lists:reverse(Acc),
%    io:format("Changes after Answers: ~p~n",[Res]),
%    Res;
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
		 {update_bindings,NewBindings} ->
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
		      
		 _ ->
		     io:format("Unknown Answer: ~p~n",[Answer]),
		     exit(avalor),
		     []
	     end,
   % io:format("Result Process Answers: ~p~n",[Result]),
    Result.
%    case Result of
%	[]->
%	    processAnswers(Intention,Answers,Acc);
%	_ ->
%	    processAnswers(Intention,
%			   Answers,lists:append(Result,Acc))
 %   end.




applyChanges(Agent,[]) ->
%   io:format("Final Agent: ~p~n",[Agent]),
   Agent;
applyChanges( #agentRationale{events = Events,
			     intentions = Intentions,
			     belief_base = BB}=Agent,
	     [Change |Changes])->

%      io:format("Change: ~p~nThen: ~p~n",[Change,Changes]),

    NewAgent = case Change of
		   {deleteIntention,_}->
		       Agent;
		   {addIntention,NewIntention} ->
		  %     io:format("Add: ~p~n",
		%		 [NewIntention]),
		       Agent#agentRationale{intentions = [NewIntention|Intentions]};

		   {addIntention,NewIntention} ->
		  %     io:format("Add: ~p~n",
		%		 [NewIntention]),
		       Agent#agentRationale{intentions = [NewIntention|Intentions]};

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
		       Agent#agentRationale{events = [Change|Events]}

	       end,
%    io:format("Next Changes: ~p~n",[Changes]),
    applyChanges(NewAgent,Changes);
applyChanges(Agent,Changes) ->
%    io:format("Agent: ~p~nChanges: ~p~n",[Agent,Changes]),
    exit(error).
					 

wrap(_,[])->
    [];
wrap(Element,List)->
    F = fun (X) -> {Element,X} end,
    lists:map(F,List).
		   
			

selectEvent([])->
    {?NULL,?NULL};
selectEvent([Event|Events]) ->
    {Event,Events}.
 


%% Default option selection functions.
%% It returns the first applicable plan computed.
selectPlan([])->
    [];
selectPlan(ItPlans) ->
    case ItPlans() of
	{Plan,_} -> 
	  %  io:format("PLAN SELECT: ~p~n",[Plan]),
	    Plan;
	false ->
	    []
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

check_mailbox(Agent=#agentRationale{events = Ev,
				   belief_base = BB})->
    Message = gatherOneMessage(),
    NewAgent = processMessage(Agent,BB,Message),
    %NewAgent = Agent#agentRationale{events = Ev++NewEvents},
    %io:format("NewAgent: ~p~n",[NewAgent]), 
    NewAgent.


% Only one message is processed in each iteratioarch2@avalor-laptop.fi.upm.esn.
% Specified in Jason Book

gatherOneMessage() ->
    receive
	Message ->
	    Message
    after 0 ->
	    []
    end.

%processMessage(Agent,BB,List)->
%    processMessages(Agent,BB,List,[]).



%processMessages(_Agent,_BB,[],Acc)->
%    lists:reverse(Acc);
processMessage(Agent = #agentRationale{events = Events,
				       module_name = Module},
	       BB,Message)->
%    case Message of
%	[]->
%	    ok;
%	_ ->
%	    io:format("\n[~p] at ~p receives Message: ~p~n",[Module,
%				         erlang:now(),Message])
 %  end,

    NewAgent = 
	case Message of

	    {'DOWN', _Ref,process, DeadProc, Reason} = DownMessage->
		%		io:format("DOWNMESSAGE: ~p~n",[DownMessage]),
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
	    
	    
	     {communication, Sender,Arch,{tell,Content}}->
	%	io:format("Message: ~p~n",[Message]),
		NewAnnot =
		    [{source,
		      {{Sender,{},[{container,{Arch},[]}]}},
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
		      {{Sender,{},[{container,{Arch},[]}]}},
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
	    
	    {communication, Sender, Arch, {untell,Content}}->
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
	    {terminate,kill,TestPid} ->
		Agent#agentRationale{events = [Message|Events]};
	    [] ->
		Agent
	end,
   
    NewAgent.

%    processMessages(Agent,BB,List,[NewEvent|Acc]).

