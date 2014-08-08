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
