
-record(monitor_task,
	{id = no_id,
	 monitoring_agent = no_monitoring_agent,
	 monitored_agent = no_monitored_agent,  
	 agent_found_result = no_agent_found_result,
	 monitored_container = no_monitored_container,

	 monitored_container_notified = false, %% register_agent_req sent?
	 monitored_container_confirmed = false, %% register_agent_resp received?

	 options = no_options, %% #monitor_options
	 pending_notification = false, 
	 %% Used when the register_monitor_request is sent due to
	 %% an agent_creation.

	 answerTo = no_answer_to
	 
	}).


-record(find_agent_task, %% ALWAYS descends from  another task
	{id = no_id,
	 parent= no_parent,
	 agent_name = no_agent_name,
	 result = no_result}).


-record(demonitor_task,
	{id = no_id,
	 monitoring_agent = no_monitoring_agent,
	 monitored_agent = no_monitored_agent,
	 monitored_container = no_monitored_container,

	 monitored_container_notified = false, %% unreg_monitor_req sent?
	 monitored_container_confirmed = false, %% unreg_monitor_resp received?
	 
	 monitor_retransfer_required = false, %% shall SM hand the monitor back?
	 dm_retransfer_sent = false, %% retransfer_monitor_req sent?
	 dm_retransfer_confirmed = false, %% retransfer_monitor_resp received?

	 answerTo = no_answer_to
	 
	}).


%% Task to erase a dead agent (made a task to receive an ack from DM)
-record(erase_agent_task,
	{id = erlang:now(),
	 dead_agent = no_agent}).
	


%% Task to notify SM-A about a change of state for a monitored agent
%% in SM-B (Added to ensure the reception by SM-A)
-record(notify_monitor_task,
	{id = erlang:now(),
	 monitored_agent = no_monitored_agent,
	 monitoring_agent = no_monitoring_agent,
	 monitoring_container = no_monitoring_container,
	 notification = no_notification,
	 relation_persist = no_relation_persist}).

%% Task to kill an agent (irrespective to its subsequent revival)
-record(kill_agent_task,
	{
	  id = erlang:now(),
	  dying_agent_name = no_dying_agent_name,
	  container = ?NOCONTAINER,
	  killing_agent_name = no_killing_agent_name 
	}).

%% Action to terminate an agent (irrespective to its subsequent revival)
-record(terminate_agent_action,
	{
	  id = erlang:now(),
	  dying_agent_name = no_dying_agent_name,
	  killing_agent_name = no_killing_agent_name,
	  killing_container = no_killing_container
	}).
