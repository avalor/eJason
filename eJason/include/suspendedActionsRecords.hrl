-define(SENDACTION, suspended_send).


-record(suspendedSend,
	%intention = no_intention,
	{my_name = no_my_name,
	performative = no_performative,
	message = no_message}).

-record(suspendedConnect,
	{ container = no_container
	 }).

-record(suspendedDisconnect,
	{ container = no_container
	 }).

-record(suspendedCreateAgent,
	{ agent_name = no_agent,
	  code = no_code,
	  container = no_container
	 }).

-record(suspendedKillAgent,
	{agent_name = no_agent}).

-record(suspended_sm_monitor,
	{request = no_request}).
