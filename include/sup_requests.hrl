%-record(create_agent_request,
%	id,
	

-record(sm_mon_request,
	{type = no_type, %MONITOR, MONITORED, REGMONITOR,DEMONITOR
	 id = erlang:now(), % time stamp
	 answerTo = no_answerTo, % pid or container
	 monitoring_agent = no_monitoring, %AgentName
	 monitored_agent = no_monitored,  %AgentName
	updated = erlang:now()
	}).

-record(sm_mon_response,
	{type, %
	id = erlang:now(), % time stamp
	sent_by, % pid or container
	%info, % Contains the info of the request e.g. the agent created
	 result,
	updated = erlang:now()
	}).


-record(sm_demon_request,
	{id = erlang:now(), % time stamp
	 answerTo = no_answerTo, % pid or container
	 monitoring_agent = no_monitoring, %AgentName
	 monitored_agent = no_monitored,  %AgentName
	updated = erlang:now()
	}).

-record(sm_demon_response,
	{
	id = erlang:now(), % time stamp
	sentBy, % pid or container
	%info, % Contains the info of the request e.g. the agent created
	 result,
	updated = erlang:now()
	}).

-record(sm_erase_mon_request,
	{
	  id = erlang:now(), % time stamp
	  monitoring_agent = no_monitoring, %AgentName
	  monitored_agent = no_monitored,  %AgentName
	  updated = erlang:now()
	 }).



