%-define(SENDACTION, suspended_send).



%% -record(suspended_connect_send,
%% 	%% Action that executes a connect, followed by send
%% 	{my_name = no_my_name,
%% 	 receiver_name = no_receiver_name,
%% 	 container = no_container,
%% 	 performative = no_performative,
%% 	 message = no_message}).


-record(suspended_send,
	%intention = no_intention,
	{my_name = no_my_name,
	 receiver_name = no_receiver_name,
	 container = no_container,
	 performative = no_performative,
	 message = no_message}).

-record(suspended_connect,
	{ container = no_container
	 }).

-record(suspended_disconnect,
	{ container = no_container
	 }).

-record(suspended_create_agent,
	{ agent_name = no_agent,
	  code = no_code,
	  anonymousVar="", %% #var Only for Anonymous agent creation
	  use_bindings=[], %% Only for Anonymous agent creation
	  container = no_container
	 }).

-record(suspended_find_agent,
	{agent_name = no_agent}).

-record(suspended_find_container,
	{ agent_name = no_agent,
	  container_var =no_container_var,
	  use_bindings = no_bindings
	 }).


-record(suspended_get_containers,
	{
	  containers_var =no_containers_var,
	  use_bindings = no_bindings
	 }).

%%%%%
%%%  Suspended actions related to supervision:
%%%%%

-record(suspended_monitor,
	{ 
	  monitoring_agent = no_monitoring_agent,
	  monitored_agent = no_monitored_agent,  
	  options = no_options
	 }).

-record(suspended_demonitor,
	{ 
	  monitoring_agent = no_monitoring_agent,
	  monitored_agent = no_monitored_agent
	 }).



-record(suspended_kill_agent,
	{dying_agent_name = no_dying_agent,
	 killing_agent_name = no_killing_agent}).


-record(suspended_supervise,
	{ 
	  supervisor_agent = no_monitoring_agent,
	  supervised_set = no_monitored_agent,  
	  supervision_policy = no_supervision_policy
	}).
