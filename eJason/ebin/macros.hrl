
-define(BINARYOPERATORS,
	[rel_lt,rel_gt,rel_le,rel_ge,rel_eq,rel_diff,
	 rel_assig,rel_decomp,arith_plus,arith_minus,
	 arith_mult,arith_slash,arith_div,arith_mod,
	 log_and,log_or]).

-define(ATOMTERMS,
	[atom,string,number]).

-define(BANGSENTENCE,[add_belief,add_achievement_goal]).

-define(ADDBELIEF,add_belief).

-define(REMOVEBELIEF,remove_belief).

-define(REMOVEADDBELIEF,remove_add_belief).

-define(ADDACHGOAL,add_achievement_goal).

-define(ADDINTENTIONGOAL,new_intention_goal).

-define(ADDTESTGOAL,add_test_goal).

-define(FAILEDACHGOAL, failed_achievement_goal).

-define(FAILEDTESTGOAL, failed_test_goal).

-define(FAILEDPLAN,plan_failed).

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


%% Responses to requests


-define(CREATED,ejason_created_agent). % response to a create_agent request

-define(NOAGENT,ejason_no_agent). % response to an is_agent request

-define(AGENTFOUND,ejason_agent_found). % response to an is_agent request

-define(NAMECLASH, ejason_name_clash).% response to a create_agent request

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



%% Types of monitoring/supervision management requests/responses

-define(MONITOR, ejason_monitor). % Sent from monitoring agent to its SM

-define(DEMONITOR, ejason_demonitor).% Sent from monitoring agent to its SM

-define(REGMONITOR, ejason_register_monitor). 
    %sent from the SM of the monitoring agent to the SM of the monitored

-define(MONITORED, ejason_monitored). 
    % Sent from the SM of the monitored to the DM of the monitored.
