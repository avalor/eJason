
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
	 
	 %% monitor_retransfer_required = false, %% shall SM hand the monitor back?
	 %% dm_retransfer_sent = false, %% retransfer_monitor_req sent?
	 %% dm_retransfer_confirmed = false, %% retransfer_monitor_resp received?

	 answerTo = no_answer_to
	 
	}).


%% Task to erase a dead agent (made a task to receive an ack from DM)
-record(erase_agent_task,
	{id = erlang:timestamp(),
	 dead_agent = no_agent}).
	


%% Task to notify SM-A about a change of state for a monitored agent
%% in SM-B (Added to ensure the reception by SM-A)
-record(notify_monitor_task,
	{id = erlang:timestamp(),
	 monitored_agent = no_monitored_agent,
	 monitoring_agent = no_monitoring_agent,
	 monitoring_container = no_monitoring_container,
	 notification = no_notification,
	 relation_persist = no_relation_persist}).

%% Task to kill an agent (irrespective to its subsequent revival)
-record(kill_agent_task,
	{
	  id = erlang:timestamp(),
	  dying_agent_name = no_dying_agent_name,
	  container = ?NOCONTAINER,
	  killing_agent_name = no_killing_agent_name 
	}).


-record(supervision_task,
	{id = no_id,
	 supervisor_agent = no_supervisor_agent,
	 supervisor_container = no_supervisor_container,
	 supervised_set= no_supervised_set,  
	 supervision_policy = no_supervision_policy,
	 pending_agents = no_pending_agents, %% Agents in the supervised set

	 %% Convenience that maps the containers of the agents in te supset.
	 %% Given a supset [a,b,c] it may be [cont1,cont2,cont1] if a and c
	 %% reside in cont1 and c in cont2
	 confirmed_agents = no_confirmed_agents,
	 result = no_result,

	 answerTo = no_answer_to
	}).



%% Action to terminate an agent (irrespective to its subsequent revival)
%% ACTIONS are processed by the SM local to the agent.
-record(terminate_agent_action,
	{
	  id = erlang:timestamp(),
	  dying_agent_name = no_dying_agent_name,
	  killing_agent_name = no_killing_agent_name,
	  killing_container = no_killing_container,
	  reason = no_reason
	}).


