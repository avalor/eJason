-record(monitor_request, %% from A (monitoring) to SM-A
	{id = erlang:now(), % time stamp
	answerTo = no_answerTo,
	 options = no_options, %% #monitor_options
	monitoring_agent = no_monitoring, %AgentName
	monitored_agent = no_monitored  %AgentName
       }).

-record(register_monitor_request, %% from SM-monitoring to SM-monitored
	{id = erlang:now(), % time stamp
	answerTo = no_answerTo, 

 options = no_options, %% #monitor_options
	 
	monitored_agent = no_monitored,  %AgentName
	monitoring_agent = no_monitoring, %AgentName

	monitoring_container = no_monitoring_container %Container
       }).

-record(demonitor_request, %% from A (monitoring) to SM-A
	{id = erlang:now(), % time stamp
	answerTo = no_answerTo, 
	monitoring_agent = no_monitoring, %AgentName
	monitored_agent = no_monitored  %AgentName
       }).

-record(unregister_monitor_request, %% from SM-monitoring to SM-monitored
	{id = erlang:now(), % time stamp
	answerTo = no_answerTo, 
	monitored_agent = no_monitored,  %AgentName
	monitoring_agent = no_monitoring, %AgentName
	monitoring_container = no_monitoring_container %Container
       }).


-record(monitor_notification_request, %% from SM-B to SM-A to inform a change
	{id = erlang:now(),
	answerTo = no_answerTo, %% Container of SM-B
	monitoring_agent = no_monitoring,
	monitored_agent = no_monitored,
	notification = no_notification, %% restart/revival/noconnection...
	relation_persist = no_relation_persist %% true/false
	}).


-record(kill_agent_request, %% from agent A to SM-A
	{
	  id = erlang:now(),
	  dying_agent_name = no_dying_agent_name,
	  %%dying_container = no_dying_container,
	  killing_agent_name = no_killing_agent_name
	  %%killing_container = no_killing_container
	}).

-record(terminate_agent_request, %% from SM-A to SM-B
	{
	  id = erlang:now(),
	  answerTo = no_answerTo, %% container of SM-A
	  dying_agent_name = no_dying_agent_name,
	  dying_container = no_dying_container,
	  killing_agent_name = no_killing_agent_name,
	  killing_container = no_killing_container
	}).
