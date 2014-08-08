%% Records useful ONLY for parsing.
%%


%-record(belief, {name,
%		 arguments={},
%		 annotations=[]).

-define(BODYFORMULA, 'EJASONBODYFORMULA').


%% -record(predicate, {name, % Name is a variable to allow higher order
%% 		    arguments = {},%tuple of var records
%% 		    annotations = [], % List of variables
%% 		    is_ground = false,
%% 		    bindings =[], %% used to bind initial goals
%% 		    unbound_vars = [] }).


	
-record(binary_operation,
	{operator = no_operator, 
	 left_part = no_left_part, 
	 right_part = no_right_part}).

-record(parsing_struct,
	{is_ordered = false,
	 counter = 0,
	 info = [], %% Used to store a diff. private info in each function
	 accum = [],
	 plan_base = [], %% Orddict that will represent the plan base
	 new_vars = []}). %% new vars that shall be added to bindings


%% -record(reason,
%% 	{belief_base = [],
%% 	 rule_base = []}).



-record(unparsed_rule, {head = no_rule_head,
			body = [],
			bindings = []}).

-record(parsed_rule, {name = no_name,
		      trigger = no_trigger,
		      context = [],
		      bindings = [],
		      first_condition = no_first_condition}).
     
       

-record(action, { type = {}, % add_belief, add_test_goal, arithmetic...
		  package = "", % only for external actions
		  body = {} }). %belief, goal, arithmetic expression


-record(unparsed_plan, {
	  trigger = no_trigger,
	  context = no_binary_operation,
	  formulas = [],
	  bindings = []}).


