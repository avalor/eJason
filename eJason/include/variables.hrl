
-define(UNBOUNDVAR,{}).

-record(var,
	{name = 'unnamed', %% Name (given by eJason parser to atoms or given in Jason code)
	 timestamp = erlang:now(), %% Used to avoid collisions
	 bind = ?UNBOUNDVAR,
	 is_ground = false,
	 unbound_vars=[] % only if it is not ground yet	 
	 }).

