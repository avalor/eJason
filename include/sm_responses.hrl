-record(monitor_response,
	{id = no_id,
	 sent_by = node(),
	 monitored_agent = no_monitored_agent,
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
	 result = no_result}). %% ?EJASONOK

-record(unregister_monitor_response,
	{id = no_id,
	 sent_by = node(),
	 monitored_agent = no_monitored_agent,
	 result = no_result}). %% ?EJASONOK

-record(monitor_notification_response, %% from SM-A to SM-B to ack a change
	{id = erlang:now(),
	monitoring_agent = no_monitoring,
	monitored_agent = no_monitored,
	notification = no_notification %% restart/revival/noconnection...
	}).


-record(monitor_notification, %% from SM-A to monitoring agent
	{ 
	  monitored_agent = no_monitored_agent,
	  prior_notification = no_specified_prior_notification,
	  notification = no_notification}).

-record(kill_agent_response, %% from SM-A to agent A
	{
	  id = erlang:now(),
	  sent_by = node(),
	  dying_agent_name = no_dying_agent_name,
	  killing_agent_name = no_killing_agent_name,
	  result = no_killing_result %% ?NOAGENT or ?EJASONOK
	}).

-record(terminate_agent_response, %% from SM-B to SM-A
	{
	  id = erlang:now(),
	  sent_by = node(), %% container of SM-B
	  dying_agent_name = no_dying_agent_name,
	  killing_agent_name = no_killing_agent_name,
	  result = no_killing_result %% ?NOAGENT or ?EJASONOK	  
	}).
