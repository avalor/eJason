%% Requests issued/processed by DM and SM

-record(start_monitor_request, %% from DM to SM (when agent is created)
	{id = erlang:timestamp(), % time stamp
	 answerTo = no_answerTo, 
	 monitored_agent = no_monitored,  %AgentName
	 agent_code = no_agent_code
	}).


-record(erase_agent_request, %% from SM to DM, (when agent dies)
	{id = erlang:timestamp(), % time stamp
	 answerTo = no_answerTo,
	 dead_agent = no_agent  %AgentName
	}).


-record(find_agent_request, %% agent/SM to DM
	{id = erlang:timestamp(),
	 answerTo = no_answer_to,
	 agent_name = no_name}).

-record(agent_creation_notification, %% From DMa to ALL DM in the system
	{id = erlang:timestamp(),
	 agent_name = no_name,
	 container = node(),
	 answerTo = node()}).

-record(agent_start_request, %% from SMb to DMb (both local to the agent)
	{id = erlang:timestamp(),
	 agent_name = no_name,
	 answerTo = node()}).



%%% ITS RESPONSE COUNTERPART IS IN SM_RESPONSES!
-record(supervise_agents_request, %% from SM-A to all SM
	{id = erlang:timestamp(), % time stamp
	 answerTo = no_answerTo,
	 supervised_set = no_supervised_set  %AgentName
	}).

