-record(agentRationale, {events=[], agentName="", 
			 intentions = [],
			 plans = [],
			 belief_base = undefined,
			 eventSelector = fun reasoningCycle:selectEvent/1, 
			 optionSelector = fun reasoningCycle:selectPlan/1, 
			 intentionSelector = fun reasoningCycle:selectIntention/1}).

-record(event, {type = external, body = {}, relatedIntention=undefined}).
-record(plan, {
     trigger = fun(_X)-> false end, %% Function to match a plan trigger
     context = fun(_X)->[] end,%% Function to match a plan context
     body = [] }).%% A list of {function,bindings} pairs. Each function executes
	       %% a plan body formula, the bindings state the positions in the
               %% variables list for the plan that are used by the function. 

-record(siPlan, %% Semi Instantiated Plan
	{event = #event{},
	plan = #plan{},
	valuation = {},
	formulas = []}).
