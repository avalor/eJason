-record(monitor_request, %% from A (monitoring) to SM-A
	{id = erlang:timestamp(), % time stamp
	answerTo = no_answerTo,
	 options = no_options, %% #monitor_options
	monitoring_agent = no_monitoring, %AgentName
	monitored_agent = no_monitored  %AgentName
       }).

-record(register_monitor_request, %% from SM-monitoring to SM-monitored
	{id = erlang:timestamp(), % time stamp
	 answerTo = no_answerTo, 
	 
	 options = no_options, %% #monitor_options
	 
	 monitored_agent = no_monitored,  %AgentName
	 monitoring_agent = no_monitoring, %AgentName
	 monitoring_container = no_monitoring_container, %Container
	 prior_notification = no_prior_notification 
	 %% Maybe the monitoring relation already existed and the agent died
       }).

-record(demonitor_request, %% from A (monitoring) to SM-A
	{id = erlang:timestamp(), % time stamp
	answerTo = no_answerTo, 
	monitoring_agent = no_monitoring, %AgentName
	monitored_agent = no_monitored  %AgentName
       }).

-record(unregister_monitor_request, %% from SM-monitoring to SM-monitored
	{id = erlang:timestamp(), % time stamp
	 answerTo = no_answerTo, 
	 monitored_agent = no_monitored,  %AgentName
	 monitoring_agent = no_monitoring, %AgentName
	 monitoring_container = no_monitoring_container %Container
%%	 monitored_container = no_monitored_container
       }).


-record(monitor_notification_request, %% from SM-B to SM-A to inform a change
	{id = erlang:timestamp(),
	 answerTo = no_answerTo, %% Container of SM-B
	 monitoring_agent = no_monitoring,
	 monitored_agent = no_monitored,
	 notification = no_notification, %% restart/revival/noconnection...
	 relation_persist = no_relation_persist, %% true/false
	 monitored_container = no_monitored_container
	}).


-record(kill_agent_request, %% from agent A to SM-A
	{
	  id = erlang:timestamp(),
	  dying_agent_name = no_dying_agent_name,
	  %%dying_container = no_dying_container,
	  killing_agent_name = no_killing_agent_name
	  %%killing_container = no_killing_container
	}).

-record(terminate_agent_request, %% from SM-A to SM-B
	{
	  id = erlang:timestamp(),
	  answerTo = no_answerTo, %% container of SM-A
	  dying_agent_name = no_dying_agent_name,
	  dying_container = no_dying_container,
	  killing_agent_name = no_killing_agent_name,
	  killing_container = no_killing_container,
	  reason = no_reason
	  %% Included in case the terminate action is related to a restart
	  %% Values: ?DEAD, ?RESTART, ?REVIVE
	}).


-record(supervision_request, %% from A (supervisor) to SM-A
	{id = erlang:timestamp(), % time stamp
	 answerTo = no_answerTo,
	 supervisor_agent = no_supervisor, %AgentName
	 supervised_set = no_supervised_set,  %AgentName
	 supervision_policy = no_policy %% #monitor_options
       }).


%% NOTE: it does not need a response
-record(register_supervision_request, %% from SM-A to involved SM
	{id = erlang:timestamp(), % time stamp
	 supervisor_agent = no_supervisor, %AgentName
	 supervisor_container = no_supervisor_container, 
	 supervision_relation = no_relation
       }).
