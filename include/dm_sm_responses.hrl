	 
-record(start_monitor_response,%% from SM to DM (when agent is created)
	{id = no_id,
	 sent_by = node(),
	 monitored_agent = no_monitored_agent, %% incl. for debugging
	 result = no_result}). %% ?EJASONOK

-record(erase_agent_response, %% from SM to DM (when agent dies)
	{id = no_id,
	 sent_by = node(),
 	 dead_agent = no_agent, %% incl. for debugging
	 result = no_result}). %% ?EJASONOK

-record(find_agent_response, %% From DM to agent / SM
	{id = no_id_given,
	 result = no_result, %% ?AGENTFOUND,?NOAGENT
	 sent_by = node(),
	 agent_name = no_name,
	 agent_pid = no_pid,
	 container = no_container}).


-record(start_agent_response, %% From DM to SM due to a restart/revive
	{id = no_id,
	 agent_name = no_name,
	 result = no_result}).
