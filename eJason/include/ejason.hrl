-record(agentRationale, 
	{events=[], agentName="",
	 module_name = "",
	 intentions = [],
	 info = {0,1}, % {Iterations,WaitTime}
	 plans = [],
%	 private_belief_base= undefined, % This belief base contains the
                       % private information of the agent (name, AID,
	               % current architecture)... which can be only consulted
                       % by some internal actions. 
	 belief_base = undefined,
	 selectEvent = fun reasoningCycle:selectEvent/1, 
	 selectPlan = fun reasoningCycle:selectPlan/1, 
	 selectIntention = fun reasoningCycle:selectIntention/1}).


-record(plan, {
     trigger = fun(_X)-> false end, %% Function to match a plan trigger
     context = fun(_X)->[] end,%% Function to match a plan context
     return_params = [],
     bindings=[], %% List of all variable-bindings.
     body = [] }).%% A list of functions. 

 

-record(piPlan, %% Partially Instantiated Plan
	{event = {}, 
	plan = #plan{},
	valuation = {},
	formulas = [],
	params = []}). %% Variables in the trigger whose match must be returned

-record(formula,
	{function,
	 variables}). 
%% This way, we know all variables before executing the formula.


