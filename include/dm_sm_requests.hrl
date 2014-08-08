%% Requests issued/processed by DM and SM

-record(start_monitor_request, %% from DM to SM (when agent is created)
	{id = erlang:now(), % time stamp
	 answerTo = no_answerTo, 
	 monitored_agent = no_monitored  %AgentName
	}).


-record(erase_agent_request, %% from SM to DM, (when agent dies)
	{id = erlang:now(), % time stamp
	 answerTo = no_answerTo,
	 dead_agent = no_agent  %AgentName
	}).


-record(find_agent_request, %% agent/SM to DM
	{id = erlang:now(),
	 answerTo = no_answer_to,
	 agent_name = no_name}).

-record(agent_creation_notification, %% From DMa to ALL DM in the system
	{id = erlang:now(),
	 agent_name = no_name,
	 container = node(),
	 answerTo = node()}).
