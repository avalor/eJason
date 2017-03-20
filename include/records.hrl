-record(monitor_options,
	
	{%% Persist (i.e. monitoring relation IS NOT disabled after)
	  persist_unknown_agent = false,
	  persist_dead_agent = false,
	  persist_restarted_agent = false,
	  persist_revived_agent = false,
	  persist_unreachable_agent = false,
	  persist_created_agent = true
      
	  %% %% Demonitor (i.e. monitoring relation IS disabled after)
	  %% demonitor_unknown_agent = false,
	  %% demonitor_dead_agent = false, 
	  %% demonitor_restarted_agent = false,
	  %% demonitor_revived_agent = false,
	  %% demonitor_unreachable_agent = false
	 }).


-record(supervision_relation,
	{
	  supervisor_agent = unknown_supervisor_agent,
	  supervisor_container = unknown_supervisor_container,
	  supervised_set = unknown_supervised_set,
	  supervised_set_containers = unknown_supervised_set_containers,
	  supervision_policy= unknown_supervision_policy,
	  %% Ancestors of the supervisor_agent, to be appended at the beginning
	  %% of the ancestors of the SupSet
	  ancestors = unknown_ancestors
	}).



-record(supervision_data,
	{
	  supervisor_agent = unknown_supervisor_agent,
	  unblock_history = [], %% List of timestamps
	  restart_history = [], %% List of timestamps
	  revival_history = [] %% List of timestamps
	  }).
	  



%% -record(supervision_info,
%% 	{supervised = unknown_supervised_agent,
%% 	container = unknown_container,
%% 	ping = [], %% Shall be a list of timestamps
%% 	unblock = [],
%% 	restart = [],
%% 	revival = []
%%        }).

-record(ping_policy,
	{frequency = 15000,
	 time = 2000,
	 maxpings = 1}).

-record(unblock_policy,
	{
	  maxunblocks = 3,
	  time = infinity
	  }).

-record(restart_policy,
	{ 
	  maxrestarts = 1,
	  time = 0
	  }).
	  
-record(revival_policy,
	{ 
	  
	  maxrevivals = 1,
	  time = 0
	}).




-record(supervision_policy,
	{
	  ping = unknown_ping_policy,
	  unblock = unknown_unblock_policy,
	  restart = unknown_restart_policy,
	  revival = unknown_revival_policy,
	  restart_strategy = one_for_one,
	  no_ping = true %% When it is true, no policy is executed
	}).
	  
