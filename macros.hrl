-record(agentRationale, {events=[], agentName="", 
			 intentions = [],
			 plans = [],
			 belief_base = undefined,
			 eventSelector = fun reasoningCycle:selectEvent/1, 
			 optionSelector = fun reasoningCycle:selectPlan/1, 
			 intentionSelector = fun reasoningCycle:selectIntention/1}).

-record(event, {type = external, body = {}, relatedIntention=undefined}).
-record(plan,{trigger=fun(_X)-> false end,context, body}). %A function each
