%% USED FOR DEBUGGING:

-define(STOP,io:get_line("Enter: ")).


-define(BINARYOPERATORS,
	[rel_lt,rel_gt,rel_le,rel_ge,rel_eq,rel_diff,
	 rel_assig,rel_decomp,arith_plus,arith_minus,
	 arith_mult,arith_slash,arith_div,arith_mod, arith_power,
	 log_and,log_or, log_not,parenthesis]).

-define(ARITHOPERATORS,
	[arith_plus,arith_minus,
	 arith_mult,arith_slash,arith_div,arith_mod, arith_power]).


-define(ATOMTERMS,
	[atom,string,number]).

%-define(BANGSENTENCE,[add_belief,add_achievement_goal]).

-define(ADDBELIEF,add_belief).

-define(REMOVEBELIEF,remove_belief).

-define(REMOVEADDBELIEF,remove_add_belief).

-define(ADDACHGOAL,add_achievement_goal).

-define(ADDINTENTIONGOAL,new_intention_goal).

-define(ADDTESTGOAL,add_test_goal).

-define(ADDNOWAITTESTGOAL,add_no_wait_test_goal).


-define(FAILEDACHGOAL, failed_achievement_goal).

-define(FAILEDTESTGOAL, failed_test_goal).

%-define(FAILEDPLAN,plan_failed).

-define(UNIQUE,true).

-define(NOTUNIQUE,false).

-define(MBOXCHECKED,true).

-define(MBOXNOTCHECKED,false).

-define(NOEVENT,false).

-define(SPAWNPROCESS,spawn).

-define(NOSPAWNPROCESS,no_spawn).

-define(DEFAULTENVIRONMENT,default_environment).

-define(STUTTERACTION,stutter_action). % internal action executed OK

-define(FAIL, fail). % internal action failed to execute

-define(SUSPEND, suspend). % returned by external actions

-define(ACTIONSUSPEND, ejason_action_suspend). % returned by ?DM and ?SM actions

-define(DM, ejason_distribution_manager).

-define(SM, ejason_supervision_manager).

-define(EJASONOK,ejason_ok).

-define(UNDERSCORE, 'EJASONUNDERSCORE').

-define(DOWN, 'DOWN').

%%%%%% Monitor notifications

-define(UNKNOWN, unknown_agent).
-define(UNREACHABLE, unreachable_agent).
-define(RESTART, restart).
-define(REVIVE, revive).
-define(CREATEDNOTIFICATION, ejason_created_notification).
-define(DEAD, dead_agent).

-define(NOPRIORNOTIFICATION, ejason_no_specified_prior_notification).
-define(PERSISTANY, ejason_persist_any).

%% Used when a monitoring relation exists, but the monitored agent is
%% unknown or unreachable (and after a dead_agent)
-define(NOMONITOREDCONTAINER, ejason_no_monitored_container). 

%% Used as a placeholder when the pid of some agent is unknown 
%% (e.g. in a kill_agent_task )

-define(NOPID, ejason_unknown_pid).

%%%%%% Responses to requests


-define(CREATED,ejason_created_agent). % response to a create_agent request

-define(NOAGENT,ejason_no_agent). % response to an is_agent request

-define(AGENTFOUND,ejason_agent_found). % response to an is_agent request

-define(NAMECLASH, ejason_name_clash).% response to a create_agent request

-define(NOCODE, ejason_code_not_found).% response to a create_agent request

-define(NOCONTAINER, ejason_no_container).
%% response to e.g. a create_agent request
%% placeholder for fields where the container is not known (yet)

-define(CONNECTED, ejason_connected). % response to a connect request

-define(DISCONNECTED, ejason_disconnected). % response to a disconnect request


%% Types of distribution management requests/responses


-define(CREATEAGENT,ejason_create_agent). % from agent to DM

-define(ISAGENT, ejason_is_agent). % between connected DMs

-define(FINDAGENT,ejason_find_agent). % from agent to DM

-define(AGENTLIST,ejason_obtain_agent_list). % sent between connected DMs

-define(CONNECT, ejason_connect). % sent from an agent to a DM

-define(DISCONNECT, ejason_disconnect). % sent from an agent to a DM

-define(CHECKAGENTS, ejason_check_agents). %  sent between not connected DMs
                                           % to inform of the agent list 
-define(CONNECTSYSTEM, ejason_connect_system). % sent between not connected DMs

-define(CONNECTTO, ejason_connect_to). % Sent between connected DMs

%-define(DISCONNECTFROM, ejason_disconnect_from). % Sent between connected DMs


-define(GETINFO, ejason_get_info). % from agent to DM







%% Values included in different SM/DM requests/responses


-define(EXTERNALMONITOR, ejason_external_monitor). 
%% placeholder for a monitoring relation replicated in the monitored container


%% -define(MONITOR, ejason_monitor). % Sent from monitoring agent to its SM

%% -define(DEMONITOR, ejason_demonitor).% Sent from monitoring agent to its SM

%% -define(REGMONITOR, ejason_register_monitor). 
%%     %sent from the SM of the monitoring agent to the SM of the monitored

%% -define(MONITORED, ejason_monitored). 
%%     % Sent from the SM of the monitored to the DM of the monitored.
