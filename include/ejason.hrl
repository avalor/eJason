-record(agentRationale, 
	{events=[], 
	 retried_events = [], %% events that have been tried and were not dropped
	 agent_name="",
	 environment = "",
	 module_name = "",
	 intentions = [],
	 suspended = [], % List of suspended intentions due to external actions
	 info = {0,1,false}, % {Iterations,WaitTime,MBOXCHECKED}
	 plans = [],
	 
	 
%	 private_belief_base= undefined, % This belief base contains the
                       % private information of the agent (name, AID,
	               % current architecture)... which can be only consulted
                       % by some internal actions. 
	 belief_base = [],
	 selectEvent = fun reasoningCycle:selectEvent/1, 
	 selectPlan = fun reasoningCycle:selectPlan/1, 
	 selectIntention = fun reasoningCycle:selectIntention/1,
	 critical_section = 0}). 
%% if this value is >0, the option selector function does not select other 
%% plans apart from the current one.


-record(plan, {
	  trigger = no_trigger, %% Term that must match the event body
	  context = no_context,%% Orddict of {M,F,A} that represents the context
	  return_variables = {[],[]},%% Variables in the trigger whose match 
	  %% must be returned {ParamVars,AnnotVars}
	  %% Separation needed to build failure event

	  replacements = [], %% Replacements to be used if the plan is used to
	                     %% match values,
	  input_vars = [], %% Variables in the params.  
	  bindings=orddict:new(), %% Initial valuation.
	  body = [] }).%% A list of functions. 

 

-record(piPlan, %% Partially Instantiated Plan
	{event = {}, 
	 plan = #plan{},
	 valuation = {},
	 formulas = [], 
	 id = erlang:now(), 
	 %% all the piPlans belonging to the same intention share this unique id
	 init_bindings = []}).% Used to generate event for Plan Failure





%% -record(formula,
%% 	{function,
%% 	 variables}). 
%% %% This way, we know all variables before executing the formula.


						%	 private_belief_base= undefined, % This belief base contains the
						% private information of the agent (name, AID,
						% current architecture)... which can be only consulted
                       % by some internal actions. 

-record(agent_info, %% It contains all the info from the mental state of an 
	%% that is passed to the formulas in the plan (rules, conditions, body).

	%% This information must be taken from the corresponding agent_rationale
	{agent_name = no_name,
	module_name = no_module_name,
	environment = no_environment,
	belief_base = []}).




-record(event, {type, % ADDACHGOAL, ADDTESTGOAL... 
		body = {}, %{PlanName, type, [ModuleName], Args,Annot}
		relatedIntention=[],
		%%Bindings updated when ! is invoked
		corrected_bindings = []}).
