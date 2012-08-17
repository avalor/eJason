%% Records useful ONLY for parsing.
%%


%-record(belief, {name,
%		 arguments={},
%		 annotations=[]).


-record(predicate, {name, % Name is a variable to allow higher order
		    arguments = {},% list of var records
		    annotations = [],
		    is_ground = false,
		    bindings =[], %% TODO: erase
		    unbound_vars = [] }).


	
-record(binary_operation,
	{operator, left_part, right_part}).

-record(parsing_struct,
	{is_ordered = false,
	 counter = 0,
	 info = [], %% Used to store a diff. private info in each function
	 accum = []}).


-record(reason,
	{belief_base = [],
	 rule_base = []}).





-record(rule, {head = #predicate{},
	       body = [],
	       bindings = []}).


-record(action, { type = {}, % add_belief, add_test_goal, arithmetic...
		  package = "", % only for external actions
		  body = {} }). %belief, goal, arithmetic expression


	  


-record(event, {type, body = {}, 
		relatedIntention=undefined}).
