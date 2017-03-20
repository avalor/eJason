-record(monitor_response,
	{id = no_id,
	 sent_by = node(),
	 monitored_agent = no_monitored_agent,
	 persistence = no_persistence_info, %% true, false
	 result = no_result}). %% ?NOAGENT, ?EJASONOK
	 
-record(register_monitor_response,
	{id = no_id,
	 sent_by = node(),
	 monitored_agent = no_monitored_agent,
	 result = no_result}). %% ?EJASONOK
	 
-record(demonitor_response, %% SM to agent
	{id = no_id,
	 sent_by = node(),
	 monitored_agent = no_monitored_agent,
	 prior_notification = ejason_no_specified_prior_notification,
	 result = no_result}). %% ?EJASONOK

-record(unregister_monitor_response,
	{id = no_id,
	 sent_by = node(),
	 monitored_agent = no_monitored_agent,
	 result = no_result}). %% ?EJASONOK

-record(monitor_notification_response, %% from SM-A to SM-B to ack a change
	{id = erlang:timestamp(),
	monitoring_agent = no_monitoring,
	monitored_agent = no_monitored,
	notification = no_notification %% restart/revival/noconnection...
	}).


-record(monitor_notification, %% from SM-A to monitoring agent
	{ 
          id = no_id, %% Added to trace the source of the notification
	  monitored_agent = no_monitored_agent,
	  prior_notification = no_specified_prior_notification,
	  notification = no_notification,
	  persists = no_persistence_info}).

-record(kill_agent_response, %% from SM-A to agent A
	{
	  id = erlang:timestamp(),
	  sent_by = node(),
	  dying_agent_name = no_dying_agent_name,
	  killing_agent_name = no_killing_agent_name,
	  result = no_killing_result %% ?NOAGENT or ?EJASONOK
	}).

-record(terminate_agent_response, %% from SM-B to SM-A
	{
	  id = erlang:timestamp(),
	  sent_by = node(), %% container of SM-B
	  dying_agent_name = no_dying_agent_name,
	  killing_agent_name = no_killing_agent_name,
	  result = no_killing_result %% ?NOAGENT or ?EJASONOK	  
	}).

-record(supervision_response, %% from SM-A to agent A
	{ id = no_id,
	  supervisor_agent = no_supervisor_agent,
	  supervised_set = no_supervised_set,
	  result = no_result}).

-record(supervise_agents_response, %% from SM to SM-A
	{ id = no_id,
	  supervised_agents = no_supervised_agent, 
	  % Those residing in SM
	  container, %% Necessary to track these agents easily
	  result %% EJASONOK or EJASONERROR
	  }).
