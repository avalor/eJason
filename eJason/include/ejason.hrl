-record(agentRationale, 
	{events=[], agentName="",
	 environment = "",
	 module_name = "",
	 intentions = [],
	 suspended = orddict:new(), %% Dict of suspended intentions due to external actions
	 %% or interactions with the ?DM or ?SM
	 info = {0,1,false}, % {Iterations,WaitTime,MBOXCHECKED}
	 plans = [],
	 
%	 private_belief_base= undefined, % This belief base contains the
                       % private information of the agent (name, AID,
	               % current architecture)... which can be only consulted
                       % by some internal actions. 
	 belief_base = [],
	 selectEvent = fun reasoningCycle:selectEvent/1, 
	 selectPlan = fun reasoningCycle:selectPlan/1, 
	 selectIntention = fun reasoningCycle:selectIntention/1}).


-record(plan, {
     trigger = fun(_X)-> false end, %% Function to match a plan trigger
     context = fun(_X)->[] end,%% Function to match a plan context
     return_variables = {[],[]},%% Variables in the trigger whose match 
	                        %% must be returned {ParamVars,AnnotVars}
	                        %% Separation needed to build failure event
     bindings=[], %% List of all variable-bindings.
     body = [] }).%% A list of functions. 

 

-record(piPlan, %% Partially Instantiated Plan
	{event = {}, 
	 plan = #plan{},
	 valuation = {},
	 formulas = [], 
	 init_bindings = []}).% Used to generate event for Plan Failure

-record(formula,
	{function,
	 variables}). 
%% This way, we know all variables before executing the formula.


