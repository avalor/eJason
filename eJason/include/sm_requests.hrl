-record(sm_mon_request,
	{type = no_mon_type, % MONITOR, DEMONITOR
	id = erlang:now(),
	monitoring_ag = no_monitoring_agent,
	monitored_ag = no_monitored_agent}).
	

