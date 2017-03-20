-module(ejason_supervision_manager).

-export([start/0, monitor/3,
	 demonitor/2, kill_agent/2,
	 supervise/3]).

-include("include/macros.hrl").
-include("include/sm_requests.hrl").
-include("include/sm_responses.hrl").
-include("include/sm_tasks.hrl").
-include("include/records.hrl"). %% contains monitor_options & supervision_relation
-include("include/dm_sm_requests.hrl").
-include("include/dm_sm_responses.hrl").


-record(sm_info, % supervision info
	{
	  monitoring_relations = dict:new(),
	  %% The key are the names of the monitored agents 

	  monitored_agents = dict:new(), 
	  %% All agents in the container are monitored by the SM.
	  %% Key = Agent Name
	  %% Value = {Ref, code}

	  
	  monitored_sm = dict:new(),
	  %% All SM in the monitored agents that appear in at
	  %% least one of the monitoring relations are monitored
	  %% (Erlang). When all relations with respect to some
	  %% container are deleted, it must be demonitored.

	  %% The corresponding restart strategy is applied when they die.
	  %% The SM monitor must inform the DM about deaths



	  supervision_relations = dict:new(),
	  %% The key is the supervisor agent


	  supervision_ancestors = dict:new(),
	  %% Is a convenience to speedup the detection of loops in the
	  %% supervision tree when a new relation is created. 
	  %% Also, each agent appearing here such that its ancestor list is not empty,
	  %% already belongs to a supervised set.
	  %% For a path a -> b -> c , an entry {c, [a,b]} will be added to SM-c
	  %% Also, in SM-b an entry {b, [a]} must exist
	  


	  supervision_registry = dict:new(),
	  %% Registers the information for each of the local supervised agents (key)
	  %% e.g. a #supervisor_data record


	  %% monitoring_agents = dict:new(),
	  %% {MonitoringAgent, [MonitoredAgent]}
          %% Used as a foreign-key convenience    
	  %% tasks = dict:new(), 

	  %% These tasks are indexed by the name of the supervised agent.
	  %% Each key/value may contain several tasks (e.g. killing an agent
	  %% requested by two different sources-> the agent dies twice if it
	  %% revived in-between).
	  %% When they require other tasks like, find_agent, the task is 
	  %% duplicated within the field tasks.

	  tasks = dict:new(),


	  %% These actions are supervision actions that shall be carried out
	  %% (e.g. killing, restarting, reviving...)
	  actions = dict:new()

	 }). 



%% Notes on monitoring relations:

%% 1) The SM in the monitoring container receives the state updates from the
%%    SM of the monitored container. It then forwards this info to the 
%%    monitoring agent. 

%% 2) The SM in the monitored container does  monitor the monitored agent
%%    as a consequence of the monitoring relation. It decides whether to
%%    restart/revive and forwards the decision to the SM in the monitoring node.
-record(monitoring_relation, {
	  monitoring_agent = no_monitoring,
	  monitoring_container = no_monitoring_container,
	  monitored_agent = no_monitored,
	  monitored_container = no_monitored_container,
	  
	  options = no_options, %% #monitor_options
	  prior_notification = ?NOPRIORNOTIFICATION}). 
	  %% Necessary to delete prior "down_agent(Monitored)[reason(N)]" in the BB
	  %% Outdated in the SM of the monitored
	  
	  %%reference = no_reference}).





% Sends a message to some recipient appending the tag ?SM
% Several clauses depending on the recipient
%% TODO: change logics. Relying on Pids implies losing requests 
%%       for revived agents (e.g. rely on names, but check if they are dead)

sm_send(Recipient, Message)->
  %% {H,M,S} = erlang:time(),
  %%   [{registered_name,Myself}] = erlang:process_info(self(), [registered_name]),
  %%   io:format("~p [Time: ~p:~p:~p]~nSending Message: "++
  %% 		  "~p~n To : ~p~n~n",
  %% 	      [Myself,H,M,S,Message,Recipient]),
    send(Recipient, Message).
  

send(?DM, Message) -> %% Send a message to local ?DM
    {?DM,node()} ! {?SM,Message};
send(?SM, Message) -> %% Send a message to local ?SM
    {?SM,node()} ! {?SM,Message};
send(Pid, Message) when is_pid(Pid)-> %% Between a SM and an agent
    %io:format("~n[SM] Sending Message: ~p~n To: ~p~n~n",[Message,Pid]),
    Pid ! {?SM,Message};
%% TODO: this case should be deleted eventually. "Dangerous"
send(Container, Message) when is_atom(Container)-> % Between SMs
    %io:format("~n[SM] Sending Message: ~p~nTo: ~p~n~n",[Message,{?SM,Container}]),
    {?SM,Container} ! {?SM,Message};
send({?SM, Container}, Message) when is_atom(Container)-> % Between SMs
    {?SM,Container} ! {?SM,Message};

send(Other,Message) ->
    io:format("[SM] Invalid Recipient: ~p~nMessage: ~p~n",[Other, Message]).



start() ->
    register(?SM,self()),
    start(#sm_info{}).


%% TODO: refactor the SM into a gen_server or gen_event
start(Info = #sm_info{tasks = InfoTasks,
		      actions = InfoActions})->
    

    Message =
	receive 
	    SomeMessage ->
		SomeMessage
	end,
    
    %% io:format("~n~n [SM] PROCESSING MESSAGE: ~n~p~n",
     %%	      [Message]),
    
    LoopInfo =
	case Message of

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MONITOR request (from agent to SM)

	    {?SM, {RequestID, 
		   #monitor_request{
		     id = RequestID,
		     answerTo = StakeHolder,
		     monitored_agent = Monitored,
		     options = Options,
		     monitoring_agent = Monitoring}}} ->
		
		


		case check_exists_monitor(Info, Monitoring, Monitored) of
		    false -> % new relation
			FindAgentID = ?DM:find_agent(Monitored),

			%% TODO: avoid possible race condition if the ?SM
			%% received a 
			%% duplicated request for the same agent.
			%% Reason: monitor task is indexed under FindAgentID
			MonitorTask = 
			    #monitor_task{id = RequestID,
					  answerTo = StakeHolder,
					  monitored_agent = Monitored,
					  monitoring_agent = Monitoring,
					  options = Options
					 },
			
			NewInfo =    Info#sm_info{ %% add task
				       tasks =
				       dict:store(
					 FindAgentID,
					 MonitorTask,
					 InfoTasks)},
			process_task(NewInfo,MonitorTask);
		    
		    #monitoring_relation{} = OtherRelation-> 
			%% Monitoring Relation already exists
			NewRelation = OtherRelation#monitoring_relation{
					options = Options},

			%% Update relation with the new configuration
			NewInfo = add_monitoring_relation(Info, NewRelation),
			

			Response = #monitor_response{
				      id = RequestID,
				      monitored_agent = Monitored,
				      persistence = true,
				      result = ?UPDATED
				     },
			sm_send(StakeHolder,{RequestID,Response}),
			NewInfo

		end; %end of check_exists_monitor 
	     

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% REGISTER MONITOR request (from SMa to SMb)

	    {?SM, {RequestID,  #register_monitor_request{
				  answerTo = StakeHolder,
				  monitoring_agent = Monitoring,
				  monitored_agent = Monitored,
				  options = Options,
				  prior_notification = Prior,
				  monitoring_container = MonitoringContainer}}} ->
		
		%% The monitoring relation is replicated in the SM at monitored 
		%% container.
		
		
		NewRelation = 
		    #monitoring_relation{
		       monitoring_agent = Monitoring,
		       monitoring_container = MonitoringContainer,
		       monitored_agent = Monitored,
		       monitored_container = node(),
		       prior_notification = Prior,
		       options = Options
		      },

		
		%% io:format("1: NewRelation: ~p~n", [NewRelation]),
		NewInfo = add_monitoring_relation(
			    Info,
			    NewRelation),
		
		%% send response
		Response = #register_monitor_response{
		  id = RequestID,
		  monitored_agent = Monitored,
		  result = ?EJASONOK},
		
		sm_send(StakeHolder,{RequestID,Response}),
		NewInfo;		    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% REGISTER MONITOR response (SMb to SMa)

	    {?SM, {ResponseID,
		   #register_monitor_response{
		     id = ResponseID,
		     result = ?EJASONOK}}} ->
		
		
		case dict:find(ResponseID,InfoTasks) of
		    {ok, MonitorTask = #monitor_task{}}->
			NewTask = MonitorTask#monitor_task{
				    monitored_container_confirmed = true},
			process_task(Info,NewTask);
		    error ->
			Info
		end;
	    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UNREGISTER MONITOR request (from SMa to SMb)

	    {?SM, {RequestID,  #unregister_monitor_request{
		     %% answerTo = StakeHolder,
		     monitored_agent = Monitored,
		     monitoring_agent = Monitoring,
		     monitoring_container = MonitoringContainer}}} ->

		
		NewInfo = 
		    delete_monitoring_relation(Info,Monitoring,Monitored),
		
		%% send response
		Response = #unregister_monitor_response{
		  id = RequestID,
		  sent_by = node(),
		  monitored_agent = Monitored,
		  result = ?EJASONOK},
		
		sm_send({?SM,MonitoringContainer}, {RequestID, Response}),
		
		NewInfo;
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UNREGISTER MONITOR response (SMb to SMa)
	    %% SM-B already deleted the monitoring relation
	    {?SM, {ResponseID,
		   #unregister_monitor_response{
		      id = ResponseID,
		      result = ?EJASONOK}}} ->
		
		case dict:find(ResponseID,InfoTasks) of
		    {ok, DemonitorTask = #demonitor_task{}}->
			NewTask = DemonitorTask#demonitor_task{
				    monitored_container_confirmed = true},
			process_task(Info,NewTask);
		    error ->
			Info
		end;
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DEMONITOR (agent to SM)
	    {?SM, {RequestID, #demonitor_request{
		     id = RequestID,
		     answerTo = StakeHolder,
		     monitoring_agent = Monitoring,
		     monitored_agent = Monitored}}} ->


		DemonitorTask = #demonitor_task{
				  id = RequestID,
				  monitoring_agent = Monitoring,
				  monitored_agent = Monitored,
				  answerTo = StakeHolder},
				  
		%% DemonitorResponse =
		%%     #demonitor_response{
		%%   id = RequestID,
		%%   sentBy = node(),
		%%   result = ?EJASONOK
		%%  },
		%io:format("[SM] DEMONITOR~n"),

		NewTask =
		    case check_exists_monitor(Info,Monitoring,
					      Monitored) of
			false -> % relation did not exist, no more steps
			    %io:format("[SM] DEMONITOR~n"),
			    
			    DemonitorTask#demonitor_task{
			      monitored_container = node(),
			      monitored_container_notified = true,
			      monitored_container_confirmed = true};
			      %% monitor_retransfer_required = false, 
			      %% dm_retransfer_sent = false, 
			      %% dm_retransfer_confirmed = false};
		    
			
			%%Monitoring relation exists
			#monitoring_relation{monitoring_container = 
						 MonitoringContainer,
					     monitored_container =
						 MonitoredContainer
					    } ->
			    
			    %% io:format("[SM] MRelation exists. Monitoring"++
			    %% 		  "Container: ~p~nMonitored C: ~p~n",
			    %% 	     [MonitoringContainer, MonitoredContainer]),
			    
			    %% erlang:demonitor(Reference),
			    case MonitoredContainer of
				MonitoringContainer -> 
				    %% TransferRequired = false,
				    %% retransfer_after_demonitor(InfoMonitors,
				    %% 			   Monitoring,
				    %% 			   Monitored),

				    %io:format("[SM] MRelation same Cont.~n"),

				    %% == node(), no need to notify
				    DemonitorTask#demonitor_task{
				      monitored_container = node(),
				      monitored_container_notified = true,
				      monitored_container_confirmed = true};
				%% monitor_retransfer_required = 
				%% 	  TransferRequired, 
				%% dm_retransfer_sent = false, 
				%% dm_retransfer_confirmed = false};
				?NOMONITOREDCONTAINER -> 
				 
				    %io:format("[SM] MRelation for nonexistent agent.~n"),

				    %% == node(), no need to notify
				    DemonitorTask#demonitor_task{
				      monitored_container = node(),
				      monitored_container_notified = true,
				      monitored_container_confirmed = true};
				
				_ ->
				    %io:format("[SM] MRelation other Cont.~n"),

			    
				    %% Demonitor must be sent to SM-B
				    DemonitorTask#demonitor_task{
				      monitored_container = MonitoredContainer,
				      monitored_container_notified = false,
				      monitored_container_confirmed = false}
				      %% monitor_retransfer_required = false,
				      %% dm_retransfer_sent = false, 
				      %% dm_retransfer_confirmed = false}
			    end
		    end,
		
		process_task(Info,NewTask);
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% NOTIFY MONITOR REQUEST (from SMb to SMa)
	    {?SM, {RequestID, Req = #monitor_notification_request{
				       id = RequestID,
				       answerTo = StakeHolder,
				       monitoring_agent = Monitoring,
				       monitored_agent = Monitored,
				       monitored_container = MonitoredContainer,
				       notification = Change,
				       relation_persist = Persists}}} ->

		OldRelation = 
		    check_exists_monitor(Info,Monitoring,Monitored),
		NewInfo =
		    case OldRelation of
			#monitoring_relation{
			   prior_notification = Prior}->
			    
			    %% io:format("[SM] Stored Relation: ~p~n",
			    %% 	      [OldRelation]),

			    case whereis(Monitoring) of
				undefined ->
				    %% TODO: check whether the monitoring 
				    %% agent does not exist or it 
				    %% is being restarted/revived. Then, queue 
				    %%the notification for later
				    ok;
				Pid ->
				    %% TODO: create a notification task that 
				    %% ensures the reception of the 
				    %%       notification with an ack (e.g. in 
				    %% case the monitoring agent 
				    %%       is restarted)
				    Notification = 
					#monitor_notification{
					   id = RequestID,
					   monitored_agent = Monitored,
					   notification = Change,
					   prior_notification = Prior,
					   persists = Persists},
				    
				    sm_send(Pid,Notification)
			    end,
			    
			    
			    case Persists of
				true ->
				    
				    NewRelation = 
					case Change of 
					    %% Monitored container no
					    %% longer known (the agent
					    %% may respawn elsewhere)
					    _ when ?DEAD== Change orelse 
						   Change==?UNREACHABLE ->
						%% Delete monitored container
						OldRelation#monitoring_relation{
						  monitored_container = 
						      ?NOMONITOREDCONTAINER,
						  prior_notification = Change};
					    _ ->
						OldRelation#monitoring_relation{
						  monitored_container = 
						      MonitoredContainer,
						  prior_notification = Change}
					end,


				    %% io:format("The request is: ~p~n",[Req]),
				    %% io:format("[SM] NewStoredRelation: ~p~n",
				    %% 	      [NewRelation]),
				    %% Modify the prior notification
				    %% of the monitoring relation
				    add_monitoring_relation(Info, 
							    NewRelation);
				
				false ->
				    %% io:format("[SM] The relation is DELETED~n"),
				    delete_monitoring_relation(
				      Info,Monitoring,Monitored)
			    end;
			false ->
			    %% io:format("[SM] The monitor relation does 
			    %% not exist~n"),
			    Info
		    end,
		
		Response =
		    #monitor_notification_response{ 
		       id = RequestID,
		       monitoring_agent = Monitoring,
		       monitored_agent = Monitored,
		       notification = Change},
		
		sm_send(StakeHolder,{RequestID,Response}),
		
		
		NewInfo;
	     
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% NOTIFY MONITOR RESPONSE (from SMa to SMb)
	    {?SM, {ResponseID, Resp = #monitor_notification_response{
		     id = ResponseID,
		     monitoring_agent = Monitoring,
		     monitored_agent = Monitored,
		     notification = Change}}} ->

		%% io:format("[SM] Resp: ~p~n",[Resp]),

		case dict:find(ResponseID,InfoTasks) of
		    {ok, NotifyMonitorTask = #notify_monitor_task{}}->

			%% delete task
			NewTasks = dict:erase(ResponseID, InfoTasks),
			
			Info#sm_info{tasks = NewTasks};
		    
			  
		    error ->
			Info
		end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% KILL AGENT request (from A to SMa)

	    {?SM, {RequestID,  
		   KillAgentRequest =
		       #kill_agent_request{
			  dying_agent_name = DyingAgent}}} ->
		
		%% Queue a new kill action for DyingAgent (may be executed
		%% immediately if it is the only one pending).
		
		NewActions =
		    dict:append(DyingAgent, KillAgentRequest,
				   InfoActions),
				   
		%% Will execute the action if this is the only one queued
		execute_supervision_actions(Info#sm_info{actions= NewActions},
					    DyingAgent);
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TERMINATE AGENT request (from SMa to SMb)
	    {?SM, {RequestID,  
		   TerminateAgentRequest =
		       #terminate_agent_request{
			  dying_agent_name = DyingAgent,
			  killing_container = KillingContainer
			 }}} ->
		
		%% Queue a new kill action for DyingAgent (may be executed
		%% immediately if it is the only one pending).
		
		NewActions =
		    case node() of
			%% Put this request at the beginning
			KillingContainer ->
			    dict:update(DyingAgent,
					   fun (List) ->
						   [TerminateAgentRequest|List]
					   end,
					   TerminateAgentRequest,
					   InfoActions);
			_ ->
			    dict:append(DyingAgent, TerminateAgentRequest,
					   InfoActions)
		    end,
		%% Will execute the action if this is the only one queued
		execute_supervision_actions(Info#sm_info{actions= NewActions},
					    DyingAgent);
		  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TERMINATE AGENT response (from SMb to SMa)
	    {?SM, {ResponseID,  
		   TerminateAgentResponse =
		       #terminate_agent_response{}}} ->
		
		NewTasks = dict:erase(ResponseID, InfoTasks),
		NewInfo = Info#sm_info{tasks = NewTasks},

		case dict:find(ResponseID, InfoTasks) of
		    
		    %%% Send the response to the killing agent
		    {ok, KillTask = #kill_agent_task{ id = ResponseID}} ->
			process_action(NewInfo, TerminateAgentResponse,
				       KillTask);
		    error ->
			NewInfo
		end;
	    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SUPERVISION request (from agent to SM)

	    {?SM, {RequestID, 
		   #supervision_request{
		      id = RequestID,
		      answerTo = StakeHolder,
		      supervisor_agent = Supervisor,
		      supervised_set = SupSet,
		      supervision_policy = SupPolicy}}} ->
		
		%% Check if the supervised set contains agents already monitored (with local info) or
		%% the supervisor tries to monitor one of its ancestors

		%% FailAlreadySupervised =
		%%     check_already_supervised(SupSet, 
		%% 				Info#sm_info.supervision_ancestors), 
		FailSuperviseAncestor =
		    check_supervising_ancestor(Supervisor, SupSet,
					       Info#sm_info.supervision_ancestors),
		
		if
		    %% FailAlreadySupervised orelse
		    FailSuperviseAncestor ->
			%% The supervision relation cannot be created
			Response = #supervision_response{
				      id = RequestID,
				      supervisor_agent = Supervisor,
				      supervised_set = SupSet,
				      result = ?EJASONERROR
				     },
			io:format("HERE!~n"),
			sm_send(StakeHolder,{RequestID,Response}),
			Info;
		    true ->
			%% The supervision relation may be created. Poll the rest of SM
			Request =
			    #supervise_agents_request{
			       id = RequestID,
			       answerTo = node(),
			       supervised_set = SupSet},
			%% Forward it to all connected containers
			sm_send(?DM,
				{Request#supervise_agents_request.id,
				 Request}),
			SupervisionTask =
			    #supervision_task{
			       id = Request#supervise_agents_request.id,
			       supervisor_agent = Supervisor,
			       supervisor_container = node(),
			       supervised_set = SupSet,
			       supervision_policy = SupPolicy,
			       pending_agents = SupSet,
			       %% A list [unk, unk...] that maps the containers of the 
				   %% SupSet in-order
			       confirmed_agents = lists:duplicate(length(SupSet),
								  unknown_container),
			       
			       answerTo = StakeHolder
			      },
						
			NewInfo =    Info#sm_info{ %% add task
				       tasks =
					   dict:store(
					     SupervisionTask#supervision_task.id,
					     SupervisionTask,
					     InfoTasks)},
			NewInfo
		end;
			    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SUPERVISE_AGENTS_REQUEST request (DM to All SM)

	    {?DM, {RequestID, 
		   #supervise_agents_request{
		      id = RequestID,
		      answerTo = StakeHolder,
		      supervised_set = SupSet
		     }}}->
		
		%% io:format("[SM ~p] Supervise agents request~n",[node()]),

		%% 1) Check if at least one of the supervised agents resides in the container

		AgentsInContainer = 
		    [Name || {Name, _} <- dict:to_list(Info#sm_info.monitored_agents)],

		case lists:filter(fun (SupervisedAgent) -> lists:member(SupervisedAgent,
									AgentsInContainer) end,
				  SupSet) of
		    [] ->
			%% io:format("SupSet: ~p~nMonitoredAgents: ~p~n",[SupSet, Info#sm_info.monitored_agents]),
			%% io:format("[SM ~p] No agents in the SupSet found in the container~n",[node()]),
		
			%% Do nothing, as none of the agents involved reside in this container
			Info;
		    Agents ->
			Response =
			    case lists:dropwhile(fun (Agent) -> 
							 
							 
							 not check_already_supervised(
							       [Agent], 
							       Info#sm_info.supervision_ancestors)
						 end,
						 Agents) of
				[]->

				    %% io:format("[SM ~p] Supervision can continue for ~p ~n",
				    %% 	      [node(), Agents]),

				    %% All the agents are not already supervised
				    #supervise_agents_response{
				       id = RequestID,
				       supervised_agents = Agents,
				       container = node(),
				       result = ?EJASONOK};
				AlreadySupervised ->
				    %% At least one of the agents is supervised
				    %%io:format("[SM DEBUG] Already Supervised for ~p~n",[AlreadySupervised]),
				    #supervise_agents_response{
				       id = RequestID,
				       supervised_agents = AlreadySupervised,
				       container = node(),
				       result = ?EJASONERROR}
			    end,
			sm_send({?SM,StakeHolder},{RequestID,Response}),	
			Info
		end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SUPERVISE_AGENTS_RESPONSE response (SM to SM-A)

	    {?SM, {ResponseID, 
		   #supervise_agents_response{
		      id = ResponseID,
		      supervised_agents = SupervisedAgents,
		      container = Container,
		      result = Result
		     }}}->
		
		
		case dict:find(ResponseID, InfoTasks) of
		    %% Remove the list of "SupervisedAgents" from the list of pending ones
		    {ok, SupervisionTask = #supervision_task{id = ResponseID,
							     supervised_set = SupSet}} ->

			%% Remove those received
			NewPendingAgents =
			    lists:filter(fun(X) ->
						 not lists:member(X, SupervisedAgents) end,
					 SupervisionTask#supervision_task.pending_agents),


			%% io:format("[SM] New pending agents = ~p~n",[NewPendingAgents]),
			%% Updated the list [cont1,cont2...]
			NewConfirmedAgents =
			    match_confirmed_agents(SupervisionTask#supervision_task.confirmed_agents,
						   SupSet, SupervisedAgents, Container),

			NewResult =
			    case Result of 
				?EJASONOK -> %% The agents in the SupSet are not already supervised in "Container"
				    SupervisionTask#supervision_task.result;
				?EJASONERROR ->
				    ?EJASONERROR 
			    end,

			%% io:format("[SM] New Result= ~p~n",[NewResult]),
			
			NewTask =
			    SupervisionTask#supervision_task{
			      result = NewResult,
			      pending_agents = NewPendingAgents,
			      confirmed_agents = NewConfirmedAgents},
			process_task(Info#sm_info{ %% add task
				       tasks =
					   dict:store(
					     ResponseID,
					     NewTask,
						     InfoTasks)},
					     NewTask);
		    error -> %% This is a response to an already resolved procedure
			Info
		end;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% REGISTER SUPERVISION request (SM-A to involved SM)
	    
	    {?SM, {RequestID, 
		   #register_supervision_request{
		      id = RequestID,
		      supervision_relation = Relation
		     }}}->
		add_supervision_relation(Info, Relation);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% SIGNALS
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	    %% Ping threshold exceeded
	    {signal, {pang,Supervisor, Supervised, TimeStamp}} ->
		
		apply_unblock_policy(Info, Supervisor, Supervised, TimeStamp);
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% MESSAGES FROM ?DM
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% START MONITOR request (DMa to SMa, at creation)
	    {?DM, {RequestID,
		   #start_monitor_request{
		      id = RequestID,
		      monitored_agent = Monitored,
		      agent_code = AgentCode}}} ->	
		
		NewRef = 
		    erlang:monitor(process,{Monitored, node()}),
		
		NewInfo = 
		    Info#sm_info{ %update the monitor
		      monitored_agents =
			  dict:store(Monitored,
					{NewRef, AgentCode},
					Info#sm_info.monitored_agents)
		     },
		
		Response =
		    #start_monitor_response{
		  monitored_agent = Monitored,
		  id = RequestID,
		  result = ?EJASONOK
		 },

		%%io:format("[DM]Monitor Agent: ~p~n",[erlang:time()]),

		sm_send(?DM,{RequestID,Response}),
	       
		
		NewInfo;
	   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Agent Creation Notification (local DM to SM)
	    
	    {?DM, {_NotificationID, #agent_creation_notification{
				       container = Container,
				       agent_name = CreatedAgentName }}}->
		
		%% io:format("[SM] NOTIFICATION RECEIVED FOR AGENT UP: ~p~n",
		%% 	  [CreatedAgentName]),

		%% Send the proper agent_up notifications, and notify
		%% the monitored container, if applicable
		MonInfo = process_monitors(Info, CreatedAgentName, Container,
					   ?CREATEDNOTIFICATION),
	
		MonInfo;
	    
	    
		    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ERASE AGENT response (SM to DM) agent dead
	    {?DM, {ResponseID, 
		   #erase_agent_response{
		     id = ResponseID,
		     
		     result = ?EJASONOK}}} ->	

		case dict:find(ResponseID,InfoTasks) of
		    
		    {ok, #erase_agent_task{ id = ResponseID}}->
		     %% Erase agent task, delete it
			NewInfo = 
			    Info#sm_info{
			      tasks = dict:erase(ResponseID,InfoTasks)
			     },
			
			NewInfo;
		
		    error ->
			Info
		end;
	    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FindAgent response (from DMb to SMb)

	    {?DM, {ResponseID,
		   Response = #find_agent_response{id = ResponseID,
						   result = Result,
						   container = Container,
						   agent_name = Name}}} ->
		    
		case  dict:find(ResponseID, InfoTasks) of
		    {ok, MonitorTask =#monitor_task{
					 %% id = MonitorID,
					 monitored_agent = Name}} ->
			NewTask =
			    case Result of
				?NOAGENT ->
					MonitorTask#monitor_task{ 
					  agent_found_result = Result};
				?AGENTFOUND ->
				    MonitorTask#monitor_task{
				      agent_found_result = Result,
				      monitored_container = Container}
			    end,
			
			NewTasks =
			    dict:erase(ResponseID, InfoTasks),
			NewInfo =
			    Info#sm_info{tasks = NewTasks},
			%% dict:erase(ResponseID,
			%% 		   NewTasks)},
			
			process_task(NewInfo,NewTask);
		    {ok, KillAgentTask =#kill_agent_task{}} ->
			NewTasks =
			    dict:erase(ResponseID, InfoTasks),
			NewInfo =
			    Info#sm_info{tasks = NewTasks},


			%% The response corresponds to a supervision action
			process_action(NewInfo,
				       Response,
				       KillAgentTask);
			    
			error-> 
			    Info
		end;
	    
		


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Messages possibly triggering 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% monitor notifications & supervision actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	    
	    %% An agent has died in the container
	    
	    Down = {?DOWN,_Ref,process, {Monitored,_Node},Reason} 
	      when Monitored =/= ?SM->
		
		%% io:format("[SM] The agent ~p is down: ~p~n",
		%% 	  [Monitored,Down]),
		process_down_message(Info, Monitored, Reason);
	    

	    %% The SM of a monitored agent is unreachable/dead, then it 
	    %% will not send notifications, the node is assumed to be
	    %% disconnected 
	    Down = {?DOWN,_Ref,process, {?SM,Node},Reason}->
		
		 %% io:format("[SM] The SM at ~p is down : ~p~n",[Node, Down]),
		%% TODO: Apply supervision policy here, i.e. modify
		%% the supervised sets 		
		
		FailReason  =
		    case Reason of
			noconnection ->
			    ?UNREACHABLE;
			_ ->
			    ?DEAD
		    end,
		
		process_down_message(Info, {?SM, Node}, FailReason);

	    

	    _ ->		
		io:format("[SM DEBUG:] Invalid SM Message: ~p~n",[Message]),
		Info
	end,
    start(LoopInfo).
    
								   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% AUXILIARY FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Invokes 'process_monitoring_relation' for every relation involving
%% Agent as monitored agent.  Then, a new #notify_monitor tasks for
%% each notification.  Some relations may be deleted (if they do not
%% persist the change) or if the monitoring agent is dead/disconnected
%% Returns a new #sm_info
process_monitors(Info = #sm_info{
		   monitoring_relations = InfoMRels},
		 Agent, Container, FailReason)->
    

    NewInfo =
	case FailReason of
	    _ when FailReason == ?DEAD orelse
		   FailReason == ?DISCONNECTED ->
    %% Delete the relations where the monitoring agent is the
    %% disconnected/dead

		DeletedRelations =
		    get_relations_for_monitoring_agent(InfoMRels,
					   Agent),
		lists:foldl(
		  fun ( #monitoring_relation{
			   monitoring_agent = MonitoringAgent,
			   monitored_agent = MonitoredAgent,
			   monitoring_container = Container},
			UseInfo)->
			  delete_monitoring_relation(UseInfo, MonitoringAgent,
						     MonitoredAgent)
		  end,
		  Info,
		  DeletedRelations);
	    _ ->
		Info
	end,
    
    case dict:find(Agent,
		      NewInfo#sm_info.monitoring_relations) of
	
	error -> % the agent is not monitored  
	    %% io:format("[SM] The agent ~p is not monitored. No notification~n",
	    %% 	      [Monitored]),
	    %% io:format("[SM] Monitoring relations: ~p ~n",[InfoMRels]),
	    Info;
	{ok,Rels} ->
	    %% io:format("[SM] notifying Change: ~p~n to relations: ~p~n",
	    %% 	      [Change, Rels]),
	    
	    List = dict:to_list(Rels),	    
	    process_monitoring_relation(NewInfo,FailReason,Container,List)
    end.




%% Sends the proper notifications to the monitoring agents in the relations
%% Starts the corresponding demonitor tasks
%% Tail-recurs and returns a new #sm_info

%% TODO: check consistency of notifications, to avoid unlikely
%%       race conditions, e.g. "disconnected" followed by "restarted"
process_monitoring_relation(Info, _Change, _MonitoredContainer, [])->
    Info;
%% The monitored agent was "unknown"/"unreachable", create the proper 
%% relations in the monitored container
process_monitoring_relation(
  Info = #sm_info{tasks = InfoTasks}, 
  ?CREATEDNOTIFICATION, MonitoredContainer,
  [{Monitoring, 
    Relation = #monitoring_relation{
		  options = PersistChanges,
		  monitored_agent = Monitored,
		  monitored_container = ?NOMONITOREDCONTAINER,
		  monitoring_container = NODE}}|Rest]) when NODE == node()->

    
    %% UpdatedRelation = Relation#monitoring_relation{
    %% 			monitored_container = MonitoredContainer},
    
    %% io:format("Duplicating monitoring relation in ~p~n",[MonitoredContainer]),
    NewMonitorTask = 
	#monitor_task{id = variables:make_timestamp_string(),
		      monitored_agent = Monitored,
		      agent_found_result= ?AGENTFOUND,
		      monitored_container = MonitoredContainer,
		      monitoring_agent = Monitoring,
		      monitored_container_notified = false,
		      monitored_container_confirmed = false,
		      options = PersistChanges,
		      answerTo = ?SM},

    %% Add new monitoring task (will generate a register_monitor_request)
    NewTasks = dict:store(NewMonitorTask#monitor_task.id,
			     NewMonitorTask, 
			     InfoTasks),
    
    
    %% Send the proper register_monitor_request
    NewInfo =
	process_task(
	  Info#sm_info{tasks = NewTasks},
	  NewMonitorTask),
    %% io:format("[SM] The monitoring relation should be already duplicated~n"),
    %% Keep on
    process_monitoring_relation(NewInfo,?CREATEDNOTIFICATION,
				MonitoredContainer,Rest);

%% The monitored container is known
process_monitoring_relation(
  Info = #sm_info{tasks = InfoTasks}, Change, MonitoredContainer,
  [{Monitoring, 
    Relation = #monitoring_relation{
		  options = PersistChanges,
		  monitored_agent = Monitored,
		  monitoring_container = MonitoringContainer}}|Rest])->
     %% io:format("[SM] Notifying ~p -> \n <-to agents in relation: ~p ~n",
     %% 	      [Change, Relation]),
    
    Persists = 	check_monitor_persistence(Change, PersistChanges),
    %% io:format("[SM] The persistence of the monitor is ~p~n",[Persists]),
    
    TaskID = erlang:timestamp(),
    
    
    %% TODO: maybe avoid this step when MonitoringContainer = MonitoredContainer
    NotificationRequest = 
	#monitor_notification_request{ id = TaskID,
				       answerTo = node(),
				       monitoring_agent = Monitoring,
				       monitored_agent = Monitored,
				       notification = Change,
				       relation_persist = Persists,
				       monitored_container = MonitoredContainer
				     },
    
    sm_send({?SM, MonitoringContainer},{TaskID,NotificationRequest}),
    
    NewTask = #notify_monitor_task{ id = TaskID,
				    monitored_agent = Monitored,
				    monitoring_agent = Monitoring,
				    monitoring_container = MonitoringContainer,
				    notification = Change,
				    relation_persist = Persists},
    
    NewTasks = dict:store(TaskID,NewTask,InfoTasks),


    
    NewInfo = 
	case MonitoringContainer == node()  of 
	    %% Do not change anything if SMa = SMb, as it will be done
	    %% upon reception of the monitor_notification_request.
	    true ->
	       	Info#sm_info{tasks=NewTasks};
	    %% Update the monitoring relation, as SMa will do
	    false ->

		case Change of 
		%% If the change is a death, the monitored container
		%% deletes the record of the relation
		    _ when ?DEAD==Change ->
			delete_monitoring_relation(Info#sm_info{ 
						     tasks = NewTasks},
						   Monitoring, Monitored);
		    _ ->
			
			NewRelation = 
			    Relation#monitoring_relation{
			      prior_notification = Change},
			%% Add the new task and modify the prior notification 
			%% of the monitoring relation
			%% io:format("2 NewRelation: ~p~n",[NewRelation]),
			add_monitoring_relation(Info#sm_info{ 
						  tasks = NewTasks}, 
						NewRelation)
		end;
		    
	    false ->
		%% delete the monitoring relation and add the new task
		delete_monitoring_relation(Info#sm_info{tasks=NewTasks},
					   Monitoring,Monitored)
	end,
    process_monitoring_relation(NewInfo,Change,MonitoredContainer, Rest).

   


%% Checks if a monitoring relation already exists.
%% Return 'false', if it does not, or the monitoring record, otherwise. 
check_exists_monitor(#sm_info{monitoring_relations = MRels},
		     Monitoring, Monitored)->
    
    case dict:find(Monitored,
		      MRels) of
	error ->
	    false;
	{ok,[]} -> %all  monitoring relations where erased before
	    false; 
	{ok, MonitoringDict} -> % dict of monitors of the monitored agent
	    case dict:find(Monitoring,
			      MonitoringDict) of
		error ->
		    false;
		
		{ok, Relation} -> %relation already exists
		    Relation
	    end
    end.
		    
%% Adds a new  monitoring relation (or replaces an existing one)
%% to the info record (two dict:store needed)
add_monitoring_relation( Info = #sm_info{ monitoring_relations = MRels},
			  NewRelation = 
			     #monitoring_relation{
				monitoring_container = MonitoringContainer,
				monitoring_agent = Monitoring,
				monitored_agent = Monitored}) ->
    
    %% io:format("[SM add_monit] New Monitoring Relation stored: ~p~n",[NewRelation]),
    
    OldMonitoringRels = % add new relation for Monitored
	case dict:find(Monitored,
			  MRels) of
	    error -> % no other agent monitors "Monitored"  
		dict:new();
	    {ok,Rels} ->
		Rels
	end,

    
    %% Monitor SM-A in order to delete this relation if Monitoring is
    %%  disconnected from Monitored.
    MonitoredSM = 
	Info#sm_info.monitored_sm,
		
    NewMonitoredSM =
	case dict:find(MonitoringContainer, 
			  MonitoredSM) of
	    {ok, _} ->
		%% SM-A is already monitored
		MonitoredSM;
	    error ->
		%% Add a monitor to {SM, MonitoredCont}
		Ref =
		    erlang:monitor(process,
				   {?SM,
				    MonitoringContainer}),
		dict:store(MonitoringContainer,
			      Ref,
			      MonitoredSM)
	end,
    
    
    Info#sm_info{% add new monitoring relation
      monitored_sm = NewMonitoredSM,
      monitoring_relations =   
	  dict:store(
	    Monitored,
	    dict:store(Monitoring,NewRelation,
			  OldMonitoringRels),
	    MRels)
     }.


%% Deletes a monitoring relation
%% Update the list of monitored_sm if necessary
%% Returns an sm_info record
delete_monitoring_relation(Info = #sm_info{ monitoring_relations = MRels,
					    monitored_sm = MonitoredSM},
			   Monitoring, Monitored) ->
    
     %% io:format("Deleting ~p-~p from ~p~n",
     %% 	      [Monitoring, Monitored, MRels]),


    %% io:format("Monitored SMs: ~p~n",[MonitoredSM]),

    case dict:find(Monitored,
		      MRels) of
	error -> 
	    %% The relation was not stored, do nothing
	    Info;
	{ok, Rels} ->
	    %% There is more than one agent monitoring the same agent,
	    %% then just erase the relation and do not demonitor the
	    %% SM
	    Info#sm_info{
	      monitoring_relations = dict:store(
				       Monitored,
				       dict:erase(Monitoring, Rels),
				       MRels)}
	
	%% {ok, [{Monitoring,
	%%        #monitoring_relation{
	%% 	  monitoring_agent = Monitoring,
	%% 	  monitored_agent = Monitored,
	%% 	  monitored_container = MonitoredContainer}}]} ->
	%%     %% The last relation with Monitored is deleted, remove its
	%%     %% entry and check whether its SM-B must be demonitored
	%%     NewRels = dict:erase(Monitored, MRels),
	    
	%%     NewMonitoredSM =
	%% 	case get_relations_for_monitored_container(
	%% 	       NewRels,
	%% 	       MonitoredContainer) of
	%% 	    [] ->
	%% 		%% No more relations with that container. Demonitor it
	%% 		case
	%% 		    dict:find(MonitoredContainer, 
	%% 				 MonitoredSM) of
	%% 		    error ->
	%% 			%% The monitored cont is also the
	%% 			%% monitoring one or the monitored
	%% 			%% container was never found
	%% 			%% (e.g. uknown_agent)
	%% 			MonitoredSM;
	%% 		    {ok, Ref} ->
			    
	%% 			erlang:demonitor(Ref),
	%% 			dict:erase(MonitoredContainer, MonitoredSM)
	%% 		end;
	%% 	    _ ->
	%% 		%% There is at least one other relation with
	%% 		%% that container, then do keep the monitor to
	%% 		%% its SM

	%% 		MonitoredSM
	%% 	end,
	%%     Info#sm_info{
	%%       monitoring_relations = NewRels,
	%%       monitored_sm = NewMonitoredSM
	%%      }
    end.
        
%%     NewRelations =
%% 	case OldMonitoringRels of
%% 	    _ when [] ->
%% 		%% Erase monitored
%% 		dict:erase(Monitored,MRels);
%% 	    _ ->
%% 		%% Erase only one relation
%% 		dict:store(
%% 		  Monitored,
%% 		  dict:erase(Monitoring,
%% 				OldMonitoringRels),
%% 		  **				       MRels)
%% 	end}.
%%     Info#sm_info{
%%       monitoring_relations = case OldMonitoringRels of
%% 				 [] ->
%% 				     %% Erase monitored
%% 				     dict:erase(Monitored,MRels);
%% 				 _ ->
%% 				     %% Erase only one relation
%% 				     dict:store(
%% 				       Monitored,
%% 				       dict:erase(Monitoring,
%% 						     OldMonitoringRels),
%% **				       MRels)
%% 			     end}.


%% Returns a list of the monitoring relations such that MonitoredCont
%% is the container where the monitored agent runs
get_relations_for_monitored_container(MRels, MonitoredCont)->
    %% [{Monitored, [Relations]}]
    List =
	dict:to_list(
	  dict:map(fun (_MonitoredAgent, Dict) ->
			      %% [{Monitoring, MRelation}]
			      ListMonitoring  = dict:to_list(Dict),
			      lists:filtermap(
				fun({_Monitoring, 
				    Relation =
					#monitoring_relation{
					   monitored_container =
					       RelMonitoredCont}}) when
					  RelMonitoredCont == MonitoredCont->
					{true, Relation};
				   (_) ->
					false
				end,
				ListMonitoring)
		      end,
		      MRels)),

    %% io:format("[SM] Relations where monitored container is ~p is:~n~p~n",
    %% 	      [MonitoredCont, List]),
    %% [[Relations1], [Relations2]...]
    FilteredList =
	lists:filtermap(fun ({_, Values}) when length(Values) > 0 ->
				{true, Values};
			    (_) ->
				false
			end,
			List),
    

    lists:flatten(FilteredList).


%% Returns a list of the monitoring relations such that MonitoringCont
%% is the container where the monitoring agent runs
get_relations_for_monitoring_container(MRels, MonitoringCont)->
    List =
	dict:to_list(
	  dict:map(fun (_MonitoredAgent, Dict) ->
			      ListMonitoring  = dict:to_list(Dict),
			      lists:filtermap(
				fun({_Monitoring, 
				    Relation =
					#monitoring_relation{
					   monitoring_container =
					       RelMonitoringCont}}) when
					  RelMonitoringCont == MonitoringCont->
					{true, Relation};
				   (_) ->
					false
				end,
				ListMonitoring)
		      end,
		      MRels)),

    %% io:format("[SM] Relations where monitoring container is ~p is:~n~p~n",
    %% 	      [MonitoringCont, List]),
    FilteredList =
	lists:filtermap(fun ({_, Values}) when length(Values) > 0 ->
				{true, Values};
			    (_) ->
				false
			end,
			List),
    

    lists:flatten(FilteredList).

%% Returns a list of the monitoring relations such that MonitoringAgent
%% is the monitoring agent.
get_relations_for_monitoring_agent(MRels, MonitoringAgent)->
    List =
	dict:to_list(
	  dict:map(fun (_MonitoredAgent, Dict) ->
			      ListMonitoring  = dict:to_list(Dict),
			      lists:filtermap(
				fun({_Monitoring, 
				    Relation =
					#monitoring_relation{
					   monitoring_agent =
					       RelMonitoringAgent}}) when
					  RelMonitoringAgent == 
					  MonitoringAgent->
					{true, Relation};
				   (_) ->
					false
				end,
				ListMonitoring)
		      end,
		      MRels)),
    
    %% io:format("[SM] Relations where monitoring agent is ~p is:~n~p~n",
    %% 	      [MonitoringAgent, List]),
    FilteredList =
	lists:filtermap(fun ({_, Values}) when length(Values) > 0 ->
				{true, Values};
			    (_) ->
				false
			end,
			List),
    

    lists:flatten(FilteredList).

    



%% %% Get some monitoring relation
%% %% Returns a monitoring relation if it can be found or [] if there is no such relation
%% get_monitoring_relation(Info = #sm_info{monitoring_relations = MRels},
%% 			Monitoring,Monitored)->
    
%%    OldMonitoringRels = 
%% 	case dict:find(Monitored,
%% 			  MRels) of
%% 	    error -> % The relation does not exist
%% 		[];
%% 	    {ok,Rels} ->
%% 		Rels
%% 	end,
    
%%     case dict:find(OldMonitoringRels,Monitoring) of
%% 	[] ->
%% 	    [];
%% 	{ok,Relation} ->
		
%% 	    Relation
%%     end.


%% Checks whether a monitoring relation persists after Change
check_monitor_persistence(Change, _Opt = #monitor_options{
			    persist_unknown_agent = Unknown,
			    persist_dead_agent = Dead,
			    persist_restarted_agent = Restarted,
			    persist_revived_agent = Revived,
			    persist_unreachable_agent = Unreachable,
			    persist_created_agent = Created})->
    %% io:format("[SM] Change is ~p for options ~p~n",
    %% 	      [Change, _Opt]),

    case Change of
	?DEAD ->
	    Dead;
	?UNKNOWN ->
	    Unknown;
	?RESTART ->
	    Restarted;
	?REVIVE ->
	    Revived;
	?UNREACHABLE ->
	    Unreachable;
	?CREATEDNOTIFICATION ->
	    Created
    end.





%% Checks if any of the agents in the supervised set is already supervised
%% Returns true or false
check_already_supervised([], _)-> 
    false;
check_already_supervised([Agent|Rest], Ancestors) -> 
    
    %% io:format("[SM] Is ~p supervised by the ancestors ~p??~n",
    %% 	      [Agent, Ancestors]),
    case dict:find(Agent, Ancestors) of
	{ok, List} when length(List) > 0 ->
	    %% io:format("[SM DEBUG] Agent ~p is already supervised by ~p~n",
	    %% 	      [Agent, List]),
	    %% Some agent is already supervised
	    true;
	_ ->
	    check_already_supervised(Rest, Ancestors)
    end.
    

%% Checks if the supervisor attemps to supervise one of its ancestors
%% Returns true or false

check_supervising_ancestor(Supervisor, SupervisedSet,
			   Ancestors)->
    case dict:find(Supervisor, Ancestors) of
	{ok, List} when length(List) > 0 ->
	    case lists:dropwhile(fun (Ancestor) -> not lists:member(Ancestor, SupervisedSet) end,
				 [Supervisor|List]) of
		[] ->
		    false;
		_ ->
		    %% io:format("[SM DEBUG] Agent ~p attemps to supervise ~p, which contains its ancestors ~p~n",
		    %% 	      [Supervisor, SupervisedSet, Ancestors]),
		    true
	    end;
	_ ->
	    false
    end.
    



%% Replaces the list of containers for the confirmed supervised agents in a supervision creation task
%% e.g. [unknown, cont1, cont2, unknown] may change to [cont3, cont1,cont2,unknown] for a SupSet 
%% [ag1...] such that ag1 resides in cont3
match_confirmed_agents([Value|ConfirmedAgents], [Agent|SupSet], SupervisedAgents, Container)->
    NewValue =
	case lists:member(Agent, SupervisedAgents) of
	    true ->
		Container;
	    false ->
		%% Do not alter the content
		Value
	end,
    [NewValue|match_confirmed_agents(ConfirmedAgents,SupSet,SupervisedAgents,Container)];
match_confirmed_agents([],[], _ ,_) ->
    [].


%% Adds a new supervision relation if it does not already exist
%% Updates dict of ancestors
%% Enforces the supervision on the local agents of the supervised set
%% Returns a new sm_info record
add_supervision_relation(Info = #sm_info{supervision_relations = Relations,
					 supervision_ancestors = InfoAncestors,
					 supervision_registry = InfoSupRegistry
					}, 
			 SupervisionRelation = #supervision_relation{
						  supervisor_agent = SupAgent,
						  ancestors = SupAgAncestors,
						  supervision_policy = SupPolicy,
						  supervised_set = SupSet,
						  supervised_set_containers = SupSetContainers}) ->			 

    %% io:format("[SM ~p] Adding supervision relation:~n ~p~n",[node(), SupervisionRelation]),
    %% io:format("[SM ~p] Existing relations:~n ~p~n",[node(), Relations]),
   
    NewRelations = 
	dict:update(SupAgent,
		    fun (List) -> %% List of relation where supAg is the supervisor
			    case lists:member(SupervisionRelation, List) of
				true ->
				    %% io:format("[SM ~p] ~p  ALREADY EXISTS~n", [node(),
				    %% 					      SupervisionRelation]),
				    List;
				false ->
				    %% io:format("[SM ~p] NEW RELATION~n",[node()]),
				    [SupervisionRelation|List] 
			    end
		    end,
		    [SupervisionRelation],
		    Relations),
    
    %% Update the ancestors of the local agents in the SupSet with the SupAg and its ancestors

    ZipList =
	 lists:zip(SupSet,SupSetContainers),
    %%Node = node(),
    LocalAgents =
	lists:filter(
	  fun (LocalAgent) ->
		  lists:member({LocalAgent, node()}, ZipList)
	  end,
	  SupSet),
    
	%% [LocalAgent ||  {LocalAgent, Node} <-ZipList],

    %% io:format("[SM ~p] Local Agents:~n ~p~n",[node(), LocalAgents]),
 
    NewAncestors =
	[SupAgent | SupAgAncestors],
    	

    UpdatedAncestors = 
	lists:foldl(
	  fun (Agent, Acc) ->
		  dict:store(Agent, NewAncestors, Acc)
	  end,
	  InfoAncestors,
	  LocalAgents),
    
   %% io:format("[SM ~p] Updated Ancestors:~n ~p~n",[node(), UpdatedAncestors]),


    %% Set up the supervision on local agents
    UpdatedSupRegistry =
	lists:foldl(
	  fun (Agent, Acc) ->
		  dict:store(Agent, #supervision_data{supervisor_agent = SupAgent},
				      Acc)
	  end,
	  InfoSupRegistry,
	  LocalAgents),

    %% Start ping policy
    lists:map(
      fun (Agent) ->
	      start_ping_policy(Agent, SupAgent, SupPolicy)
      end,
      LocalAgents),
        
      
      
			     
    Info#sm_info{supervision_relations = NewRelations,
		supervision_ancestors = UpdatedAncestors,
		supervision_registry = UpdatedSupRegistry}.



%% If a ping policy is specified, it is started
%%
%% No_ping enabled, then skip
start_ping_policy(_Agent, _SupAgent,_Sup= #supervision_policy{
					     no_ping=true})->
    ok;
start_ping_policy(Agent, SupAgent,_Sup= #supervision_policy{
					   no_ping=false,
					   ping = PingPolicy}) ->
    #ping_policy{
       frequency = Frequency,
       time = PingTime,
       maxpings = MaxPings
      } = PingPolicy,
    erlang:spawn(ping_policy_watcher, start, [SupAgent, Agent, Frequency,PingTime,
					      MaxPings]).


%% Checks the unblock policy of an agent and applies it if necessary
%% Returns a new Info record
apply_unblock_policy( Info = #sm_info{
				supervision_relations = Relations,
				supervision_registry = SupRegistry},
		      Supervisor, Supervised, TimeStamp) ->
    
    %% 1) Get the unblock history
    {ok, SupData} =
	dict:find(Supervised, SupRegistry),
    #supervision_data{
	   unblock_history = History
      } = SupData,
    
    
	
    %% 2) Check the policy applied
    case dict:find(Supervisor, Relations) of
	error ->
	    io:format("[SM ERROR] Unblock policy applied for ~p a non-existent supervision relation from ~p~n",
		      [Supervised, Supervisor]),
	    Info;
	{ok, RelList} ->
	    case lists:filter(
		   fun (_Relation = #supervision_relation{
				      supervised_set = SupSet
				     }) ->
			   lists:member(Supervised, SupSet)
		   end,
		   RelList) of
		[] ->
		    io:format("[SM ERROR(2)] Unblock policy applied for ~p a non-existent supervision relation from ~p~n",
			      [Supervised, Supervisor]),
		    Info;
		[SupRel] ->
		    #supervision_relation{
		       supervision_policy = SupPolicy
		      } = SupRel,
		    #supervision_policy{
		       unblock = UnblockPolicy
		      } = SupPolicy,
		    #unblock_policy{
		       time = UnblockTime,
		       maxunblocks = MaxUnblocks
		      } = UnblockPolicy,
		    
		    UnblockHistory = [TimeStamp|
				      utils:clean_history(History, UnblockTime, TimeStamp)],
		    

		   
		    %% Either apply restart or unblock policy 
		    if 
			%% UNBLOCK POLICY (and resume pings)
			length(UnblockHistory) =< MaxUnblocks ->
			    %% Apply unblock policy and update supdata
			    try
				Supervised ! {signal, ?UNBLOCK}
			    catch
				_:_ ->
				    io:format("[SM Warning] Unblocking a dead agent: ~p~n",
					      [Supervised])
			    end,

			    NewSupData =
				SupData#supervision_data{
				  unblock_history = [TimeStamp|UnblockHistory]},
			    NewSupRegistry = dict:store(Supervised,
							NewSupData,
							SupRegistry),

			    start_ping_policy(Supervised, Supervisor, SupPolicy),

			    Info#sm_info{
			      supervision_registry = NewSupRegistry
			     };

			true ->
			    %% Flush unblock history and apply restart policy
			    NewSupData =
				SupData#supervision_data{
				  unblock_history = []},
			    NewSupRegistry = dict:store(Supervised,
							NewSupData,
							SupRegistry),
			    
			    NewInfo =
				Info#sm_info{
				  supervision_registry = NewSupRegistry
				 },
			
			    apply_restart_policy( NewInfo, Supervisor, Supervised, 
						  TimeStamp)
				
		    end
	    end
    end.

	
	  
%% Checks the restart policy of an agent and applies it if necessary
%% Returns a new Info record
apply_restart_policy( Info = #sm_info{
				monitored_agents = LocalAgents,
				supervision_relations = Relations,
				supervision_registry = SupRegistry},
		      Supervisor, Supervised, TimeStamp) ->

    io:format("Applying restart policy for ~p~n",[Supervised]),
    
    %% 1) Get the restart history
    {ok, SupData} =
	dict:find(Supervised, SupRegistry),
    #supervision_data{
	   restart_history = History
      } = SupData,
    
    %% 2) Check the policy applied
    case dict:find(Supervisor, Relations) of
	error ->
	    io:format(
	      "[SM ERROR] Restart policy applied for ~p a non-existent supervision relation from ~p~n",
	      [Supervised, Supervisor]),
	    Info;
	{ok, RelList} ->
	    case lists:filter(
		   fun (_Relation = #supervision_relation{
				      supervised_set = SupSet
				     }) ->
			   lists:member(Supervised, SupSet)
		   end,
		   RelList) of
		[] ->
		    io:format("[SM ERROR(2)] Restart policy applied for ~p a non-existent supervision relation from ~p~n",
			      [Supervised, Supervisor]),
		    Info;
		[SupRel] ->
		    #supervision_relation{
		       supervision_policy = SupPolicy
		      } = SupRel,
		    #supervision_policy{
		       restart = RestartPolicy
		      } = SupPolicy,
		    #restart_policy{
		       time = RestartTime,
		       maxrestarts = MaxRestarts
		      } = RestartPolicy,
		    
		    RestartHistory = [TimeStamp|
				      utils:clean_history(History, RestartTime, TimeStamp)],
		    

		   
		    %% Either restart the agent or kill the agent (sup. relation is not  disabled)
		    if 
			length(RestartHistory) =< MaxRestarts ->
			    %% Get the code for the agent
			    {ok, {_Ref, AgentCode}} =
				dict:find(Supervised, LocalAgents),
			    
			    %% Apply restart policy and update supdata
			    %% Ping policy will be resumed after restart from SupRel
			    exit(whereis(Supervised),
				  {?SM,  {?RESTART, AgentCode, SupRel}}),
			    
			    NewSupData =
				SupData#supervision_data{
				  restart_history = [TimeStamp|RestartHistory]},
			    NewSupRegistry = dict:store(Supervised,
							NewSupData,
							SupRegistry),


			    Info#sm_info{
			      supervision_registry = NewSupRegistry
			     };

			true -> %% Kill the agent
			    %% io:format("[SM] Do not restart ~p~n",
			    %%   [Supervised]),

			    %% TODO: disable SupRel.
			    exit (whereis(Supervised),
				  {?SM, ?DEAD}),
			    Info
		    end
	    end
    end.


%% Checks the revival policy of an agent and determins if it must be revived
%% Returns {NewInfo, {?REVIVE, AgentCode, SupRel}|?DEAD}. 
%%The second element in the tuple determines if the agent will be revived later on 
apply_revival_policy( Info = #sm_info{
				monitored_agents = LocalAgents,
				supervision_relations = Relations,
				supervision_registry = SupRegistry},
		      Supervised)->
    TimeStamp = utils:timestamp(),
    %% 1) Get the revival history
    case dict:find(Supervised, SupRegistry) of
	error ->
	    %% Agent not supervised

	    {Info, ?DEAD};
	

	{ok, SupData} ->
	    #supervision_data{
	       supervisor_agent = Supervisor,
	       revival_history = History
	      } = SupData,
	    
    
	    %% 2) Check the policy applied
	    case dict:find(Supervisor, Relations) of
		error ->
		    io:format(
		      "[SM ERROR] Revival policy applied for ~p:++"
		      "non-existent supervision relation from ~p~n",
		      [Supervised, Supervisor]),
		    {Info, ?DEAD};
		{ok, RelList} ->
		    case lists:filter(
			   fun (_Relation = #supervision_relation{
					       supervised_set = SupSet
					      }) ->
				   lists:member(Supervised, SupSet)
			   end,
			   RelList) of
			[] ->
			    io:format(
			      "[SM ERROR(2)] Revival policy applied for"++
				  " ~p: non-existent supervision relation from ~p~n",
			      [Supervised, Supervisor]),
			    {Info, ?DEAD};
			[SupRel] ->
			    #supervision_relation{
			       supervision_policy = SupPolicy
			      } = SupRel,
			    #supervision_policy{
			       revival = RevivalPolicy
			      } = SupPolicy,
			    #revival_policy{
			       time = RevivalTime,
			       maxrevivals = MaxRevivals
			      } = RevivalPolicy,
			    
			    RevivalHistory = [TimeStamp|
					      utils:clean_history(History, RevivalTime, TimeStamp)],
			    
			    
			    %% Determine what shall be done to the agent 
			    if 
				%% Revive it
				length(RevivalHistory) =< MaxRevivals ->
				    %% Get the code for the agent
				    {ok, {_Ref, AgentCode}} =
					dict:find(Supervised, LocalAgents),
				    
				    %% Apply revival policy and update supdata
				    
				    NewSupData =
					SupData#supervision_data{
					  unblock_history = [],
					  restart_history = [],
					  revival_history = [TimeStamp|RevivalHistory]},
				    NewSupRegistry = dict:store(Supervised,
								NewSupData,
								SupRegistry),

				    {Info#sm_info{
				       supervision_registry = NewSupRegistry
				      }, {?REVIVE, AgentCode, SupRel}};
				true ->
				    {Info, ?DEAD}
			    end
		    end
	    end
    end.




%% Receives the agent that has died and executes the corresponding
%% actions (restart, revive or delete).
%% It returns the notification to be sent (?RESTART|?REVIVE|?DEAD)
execute_supervision_policy(Info = 
			       #sm_info{}, 
			   Monitored, FailReason)->  

    

    case FailReason of
	?UNKNOWN->
	    ?UNKNOWN;
	?UNREACHABLE ->
	    ?UNREACHABLE;
	{RESTARTORREVIVE, AgentCode, SupRel} when RESTARTORREVIVE == ?RESTART orelse
						  RESTARTORREVIVE == ?REVIVE->
	    
	    Supervisor = SupRel#supervision_relation.supervisor_agent,
	%% Restart the agent and refresh the ping policy
	    try
		%% io:format("[SM]~p-ing the agent ~p with code ~p ~n",
		%% 	  [RESTARTORREVIVE, Monitored, AgentCode]),
		
		InitialState = AgentCode:init_state(Monitored),
		%% io:format("[SM Restart] InitState:  ~p ~n",
		%% 	  [InitialState]),
		Pid = spawn(reasoningCycle,reasoningCycle,
			    [InitialState]),
		%% io:format("[SM Restart] Pid:  ~p ~n",
		%% 	  [Pid]),
		
		register(Monitored,Pid),
		%% NOTE: Ref in monitored_agents is outdated (not used though)
		NewRef =
		    erlang:monitor(process,{Monitored, node()}),
		%% io:format("[SM Restart] Ref:  ~p ~n",
		%% 	  [NewRef]),
		start_ping_policy(Monitored, Supervisor, 
				  SupRel#supervision_relation.supervision_policy),
		%% io:format("[SM Restart] Ping reset ~n",
		%% 	  []),

		RESTARTORREVIVE
	    catch
		Error:Reason->
		    
		    io:format("[SM DEBUG FATAL] ~p not restarted.\n",
			      [Monitored]),
		    io:format("[SM DEBUG FATAL] ~p:~p~n",
			      [Error, Reason]),
		    ?DEAD
	    end;
	_ ->
	    
	    ?DEAD
    end.
	    




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Down Messages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% Returns an updated sm_info record


%% Another SM is dead/disconnected. Notify the monitoring agents that
%% monitor some agent in SM's container. Also, delete all monitoring
%% relations where that container is the Monitoring container
process_down_message(Info= #sm_info{ monitoring_relations = MRels},
		     {?SM, MonitoredCont}, 
		     FailReason)->
    
    %% io:format("[SM] Down message for container ~p. FailReason used: ~p~n",
    %% 	      [MonitoredCont, FailReason]),

    
    %% These are the relations where the container that is down is monitored
    Relations =
     	get_relations_for_monitored_container(MRels, MonitoredCont),

    %% These are the relations where the container that is down is monitoring
    
    DeletedRelations =
	get_relations_for_monitoring_container(MRels, MonitoredCont),
    

    %% Delete the relations where the monitoring container is the
    %% disconnect one
    NewInfo =
	lists:foldl(
	  fun ( #monitoring_relation{
		  monitoring_agent = MonitoringAgent,
		  monitored_agent = MonitoredAgent,
		  monitoring_container = MonitoredCont},
		UseInfo)->
		  delete_monitoring_relation(UseInfo, MonitoringAgent,
					     MonitoredAgent)
	  end,
	  Info,
	  DeletedRelations),
       
    
     %% io:format("[SM] The relevant relations are: ~p~n",
     %% 	       [Relations]),

    %% We must invoke process_monitors ONCE for each different monitored agent
   
    %%
    RepeatedAgents = lists:map(fun (#monitoring_relation{
				       monitored_agent = SomeAgent}) ->
				       SomeAgent end,
			       Relations),

    DistinctAgents = lists:usort(RepeatedAgents),
    
    
    Fun = fun (MonitoredAgent, SMInfo) ->
		  process_monitors(SMInfo,MonitoredAgent, 
				   MonitoredCont, FailReason)
	  end,


    lists:foldl(Fun, NewInfo, DistinctAgents);

%% An agent has died in the container. Send the proper notification to the
%% monitoring agents and delete the monitoring relations where this agent
%% is the monitoring agent
process_down_message(Info= #sm_info{tasks = InfoTasks,
				    actions = InfoActions},
		     Monitored, Reason)->
    
    %% io:format("[SM] Down message for agent ~p. Reason received: ~p~n",
    %%  	       [Monitored, Reason]),

    {ActInfo, FailReason}=
	case Reason of
	    noproc ->
		{Info, ?UNKNOWN};
	    noconnection ->
	     	{Info, ?UNREACHABLE};

	    {?SM, ?DEAD}->
	    	%% The agent has exceeded the restart threshold.
	    	%% Let the agent die.
	    	{Info, ?DEAD};

	    %% TODO: include restart & revivals as actions to ensure
	    %% their execution under all interleavings. (v1.1)

	    {?SM, {?RESTART, AgentCode, SupRel}} ->
		{Info, {?RESTART, AgentCode, SupRel}};
	    
	    
	    %% The SM killed it. Process the related
	    %% actions related to supervision.
	    
	    {?SM, DeathReason, ActionID}->
		
		UpdatedInfo =
		    %% The related action is a terminate_agent task.
		    %% Then, send the proper response.
		    case dict:find(Monitored, InfoActions) of
			{ok, [Action|Actions]}->

			    %% Deprecated -- Apply revival if necessary
			    %% Info =
				%%case DeathReason of
				%% %% These actions are applied if the agent must be revived
				%% ?REVIVE ->
				%% 	io:format("[SM]Reviving agent ~p~n",[Monitored]),
				%% 	StartID = ?DM:start_agent(Monitored),
				%% 	%% TODO: avoid blocking SM
				%% 	receive 
				%% 	    #start_agent_response{
				%% 	       id = StartID,
				%% 	       agent_name = Monitored,
				%% 	       result = StartPid					  
				%% 	      } when is_pid(StartPid) ->

				%% 		NewRef = 
				%% 		    erlang:monitor(process,{Monitored, node()}),

				%% 		Info#sm_info{ %update the monitor
				%% 		  monitored_agents =
				%% 		      dict:store(Monitored,
				%% 				    NewRef,
				%% 				    Info#sm_info.monitored_agents)
				%% 		 }
				%% 	after 50000 ->
				%% 		io:format("[SM WARNING] The agent ~p was not revived~n",
				%% 			  [Monitored]),
				%% 		Info
				%% 	end;

				%%     _ ->
				%% 	Info
				%% end,
			    
			    process_action(Info, 
					   {'DOWN', Monitored, ActionID},
					   Action);	
			
			error ->
			    io:format("[SM] The agent_down message is outdated."),
			    io:format(" The deathreason was ~p~n.", [DeathReason]),
			    %% Outdated agent_down message
			    Info
		    end,
		
		%% Determine if the Agent must be revived
		apply_revival_policy(UpdatedInfo, Monitored);	    
	    _ ->
		%% The agent died by itself (not killed by an SM)
		%% Determine if the Agent must be revived
		apply_revival_policy(Info, Monitored)	    
	    end,


    %% io:format("[SM] Down message for agent ~p. Reason used: ~p~n",
    %% 	      [Monitored, FailReason]),
    %% io:format("The Erlang reason is: ~p~n",[Reason]),
    
    %% Compute supervision action according to the supervision policy
    %% ?RESTART, ?REVIVE, ?DEAD <- the agent dies


    %% io:format("[SM] The death of ~p is determined as:~p~n",
    %% 	       [Monitored, FailReason]),

    
    Notification =
	execute_supervision_policy(ActInfo, Monitored, FailReason),
	
    %% io:format("~n~n~n[SM] The NOTIFICATION for ~p is determined as:~p~n~n~n",
    %% 	       [Monitored, Notification]),
    
    MonInfo =
	process_monitors(ActInfo,Monitored,node(),Notification),
    
    ReturnInfo =
	case Notification of
	    ?RESTART ->
		%% The agent was restarted successfully
		%%
		MonInfo;
	    ?REVIVE ->
		%% The agent was revived successfully
		%%
		MonInfo;
	    
	    %% Delete the agent (it was not revived/restarted either due to a 
	    %% failure or because it is determined by the sup. policy).
	    _->
		
		TaskID = erlang:timestamp(),
		
		NewTask = 
		    #erase_agent_task{ id = TaskID,
				       dead_agent = Monitored},
		
		Request =
		    #erase_agent_request{ id = TaskID,
					  dead_agent = Monitored},
		
		sm_send(?DM,{TaskID,Request}),
		
		NewTasks = dict:store(TaskID,
				      NewTask,
				      InfoTasks),
		MonInfo#sm_info{
		  %% add new erase_agent_task
		  tasks = NewTasks}
	end,
    
    ReturnInfo.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SUPERVISION RELATED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Executes a supervision action for AgentName (e.g. kill...)
%% if the first queued action is a request. If it is a task, it is
%% already being carried out, so nothing changes. 
execute_supervision_actions(Info =
				#sm_info{actions= InfoActions},
			    AgentName)->
    
    QueuedActions =
	case dict:find(AgentName, InfoActions) of
	    {ok, List} ->
		List;
	    error ->
		[]
	end,
    
    
    case QueuedActions of
	[] ->
	    
	    %% All queued actions have been executed
	    NewActions = dict:erase(AgentName, InfoActions),
	    Info#sm_info{actions = NewActions};
	
	[#kill_agent_task{}|_] ->
	    %% Action being carried out. Do nothing
	    Info;
	[#terminate_agent_action{}|_] ->
	    %% Action being carried out. Do nothing
	    Info;

	
	%%% All options included to avoid unexpected bugs

	[#kill_agent_request{}|_] ->
	    apply_supervision_action(Info, AgentName, QueuedActions);
	[#terminate_agent_request{}|_] ->
	    apply_supervision_action(Info, AgentName, QueuedActions)
    end.
    
 
%%%%% Next pending action for AgentName is a terminate_agent.
%% Processed at SM-B (the container where the agent lives)
apply_supervision_action(Info= #sm_info{
				  actions = InfoActions}, 
			 DyingAgent,
			 [#terminate_agent_request{
			     dying_container = DyingContainer,
			     id = RequestID,
			     dying_agent_name = DyingAgent,
			     killing_container = KillingContainer,
			     killing_agent_name = KillingAgent,
			     reason = TerminateReason}|
			  RestActions]) when DyingContainer == node()->
    
    Action = 
	#terminate_agent_action{
	   id = RequestID,
	   dying_agent_name = DyingAgent,
	   killing_agent_name = KillingAgent,
	   killing_container = KillingContainer,
	   reason = TerminateReason},
    
    NewActions = dict:store(
		   DyingAgent, 
		   [Action|RestActions], InfoActions),
        
    case whereis(DyingAgent) of
	Pid when is_pid(Pid) ->
	    
	    %% Kill the agent
	    erlang:exit(Pid, {?SM, TerminateReason, RequestID}),
	    Info#sm_info{actions = NewActions};
	_ ->
	    %% The agent no longer exists here.  Answers back as if a
	    %% down message were received.
	    process_action(
	      Info#sm_info{actions = NewActions},
	      {?DOWN, DyingAgent, RequestID},
	      Action)
    end;
	      

%%%%% Next pending action for AgentName is an agent_kill.
%% Processed at SM-B (requires a find_agent_task to locate agent A)
apply_supervision_action(Info= #sm_info{tasks = InfoTasks,
					actions = InfoActions}, 
			 AgentName,
			 [#kill_agent_request{
			     id = RequestID,
			     dying_agent_name = DyingAgent,
			     killing_agent_name = KillingAgent}|
			  RestActions])->
    
    FindAgentID = ?DM:find_agent(DyingAgent),

    KillAgentTask = 
	#kill_agent_task{
	   id = RequestID,
	   dying_agent_name = DyingAgent,
	   killing_agent_name = KillingAgent},

    NewTasks =
	dict:store(FindAgentID,
		      KillAgentTask,
		      InfoTasks),

    NewActions = dict:store(AgentName,
			       [KillAgentTask|RestActions],
			       InfoActions),

    Info#sm_info{ %% add task and replace action
      tasks = NewTasks,
      actions = NewActions}.
    




%% This function processes the responses related to the different
%% supervision actions. It returns an updated sm_info. Note that, in
%% contrast to process_task, it requires an extra parameter,
%% corresponding to the response related to some task. This task, has
%% been erased from the task list within sm_info

%% A down message is received as part of the terminate_agent action within
%% a kill_agent task
process_action(Info= #sm_info{ actions = InfoActions},
	       #terminate_agent_response{
		  id = ResponseID,
		  result = Result},
	       KillTask = #kill_agent_task{
			     id = ResponseID,
			     dying_agent_name = DyingAgent,
			     killing_agent_name = KillingAgent})->
    case dict:find(DyingAgent, InfoActions) of 
	{ok, [_KillAgentTask|RestActions]} ->
	    
	    Response = #kill_agent_response{
			  id = ResponseID,
			  dying_agent_name = DyingAgent,
			  killing_agent_name = KillingAgent,
			  result = Result},
	    
	    case whereis(KillingAgent) of
		Pid when is_pid(Pid) ->
		    sm_send(Pid, {ResponseID, Response});
		_ ->
		    %% The killing agent no longer exists or is being
		    %% restarted.
		    ok
	    end,
	    NewActions = dict:store(DyingAgent, RestActions,
				       InfoActions),

	    %% Continue with the supervision actions of DyingAgent (if any)
	    execute_supervision_actions(Info#sm_info{actions = 
							 NewActions},
					DyingAgent);
	error ->
	    %% Action already erasen
	    Info
    end;

%% A down message is received as part of a terminate_agent action.

process_action(Info= #sm_info{tasks = InfoTasks,
			      actions = InfoActions},
	       {?DOWN, DyingAgent, TerminateID},
	       TerminateAction = 
		   #terminate_agent_action{
		      id = TerminateID,
		      dying_agent_name = DyingAgent,
		      killing_agent_name = KillingAgent,
		      killing_container = KillingContainer})->
    %% io:format("[SM] InfoActions: ~p~n",[InfoActions]),
    
    {ok, [TerminateAction|RestActions]}= dict:find(
					   DyingAgent,
					   InfoActions),
    
        Response = #terminate_agent_response{
		  id = TerminateID,
		  dying_agent_name = DyingAgent,
		  killing_agent_name = KillingAgent,
		  result = ?EJASONOK},
    
    
    sm_send({?SM, KillingContainer}, {TerminateID, Response}),
    
    NewActions = dict:store(DyingAgent, RestActions, 
			       InfoActions),

    Info#sm_info{actions = NewActions};


%% Result of the search for an agent being killed
process_action(Info= #sm_info{tasks = InfoTasks,
			      actions = InfoActions},
	       #find_agent_response{result = Result,
				    agent_pid = AgentPid,
				    container = Container,
				    agent_name = DyingAgent},
	       KillTask =#kill_agent_task{
			    id = KillID,
			    dying_agent_name = DyingAgent,
			    killing_agent_name = KillingAgent})->
    
    case Result of
	%% TODO: cancel subsequent actions for DyingAgent, as it does not
	%% belong to the system (deleting them does not suffice, as 
	%% there is an intention blocked in some agent for each action)
	?NOAGENT ->
	    %% The agent does not belong to the system.
	    Response = #terminate_agent_response{
			  id = KillID,
			  dying_agent_name = DyingAgent,
			  killing_agent_name = KillingAgent,
			  result = ?NOAGENT},
	    process_action(Info, Response, KillTask);

	    %% case whereis(KillingAgent) of
	    %% 	Pid when is_pid(Pid) ->
	    %% 	    sm_send(Pid, {KillID, Response});
	    %% 	_ ->
	    %% 	    %% The killing agent no longer exists or is being
	    %% 	    %% restarted.
	    %% 	    ok
	    %% end,
	    %% %% Erase all supervision actions pending
	    %% Info#info{
	    %%    actions = dict:erase(DyingAgent, InfoActions)
	    %%   };
	
	?AGENTFOUND ->
	    NewKillTask =
		KillTask#kill_agent_task{container = Container},
	    
	    TerminateRequest =
		#terminate_agent_request{
		   id = KillID,
		   answerTo = node(),
		   killing_container = node(),
		   dying_agent_name = DyingAgent,
		   dying_container = Container,
		   killing_agent_name = KillingAgent,
		   reason = ?DEAD
		  },
	    
	    sm_send({?SM, Container}, {KillID, TerminateRequest}),
	    
	    NewTasks = dict:store(KillID, NewKillTask, InfoTasks),
	    Info#sm_info{
	      tasks = NewTasks
	      }
    end.
	    
	    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Process Task (after updates), returns a new #sm_info
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% MONITOR TASK (monitored not yet found, search is ongoing)
process_task(Info = #sm_info{},
	     #monitor_task{agent_found_result= no_agent_found_result})->
    Info;
%% MONITOR TASK (monitored not found)
%% The monitoring agent is notified (= unknown_agent)
%% The relation is created, but if the agent appears
%% this relation shall be updated

%% TODO: this is mostly a clone of the ?AGENTFOUND case where
%% SMa == SMb 
process_task(Info = #sm_info{tasks = Tasks},
	     #monitor_task{id = TaskID,
			   monitoring_agent = Monitoring, 
			   monitored_agent = Monitored,
			   agent_found_result= ?NOAGENT,
			   answerTo = StakeHolder, 
			   options = Options})->
    
   %% 1) Check if the monitor relation will persist after an uknown notif.

    Persists = check_monitor_persistence(?UNKNOWN,
					  Options),

    %% 2) Add the new relation if it persists the notification

    %%(prior notification is set to "unknown")

    NewRelation = #monitoring_relation{
		     monitoring_agent = Monitoring,
		     monitoring_container = node(),
		     monitored_agent = Monitored,
		     prior_notification = ?UNKNOWN,
		     monitored_container = ?NOMONITOREDCONTAINER,
		     options = Options
		    },
    	    
    NewInfo = 
	case Persists of
	    true ->
		%% io:format("3: NewRelation:~p~n",[NewRelation]),
		add_monitoring_relation(Info,NewRelation);
	    false ->
		Info
	end,

    %% 2) Send the response
    
    Response = 
	#monitor_response{ id = TaskID,
			   monitored_agent = Monitored,
			   persistence = Persists,
			   result = ?NOAGENT},
    sm_send(StakeHolder,
	    {TaskID,Response}),
    
    NewTasks =
	dict:erase(TaskID,
		      Tasks),

   
    NewInfo#sm_info{tasks = NewTasks};

%% MONITOR TASK (monitored found)
%% Monitored SM must be notified
process_task(Info = #sm_info{tasks = Tasks,
			     monitored_sm = SMMonitored},
	     MonitorTask = 
	     #monitor_task{id = TaskID,
			   monitored_agent = Monitored,
 			   agent_found_result= ?AGENTFOUND,
 			   monitored_container = MonitoredContainer,
 			   monitoring_agent = Monitoring,
 			   monitored_container_notified = Notified,
 			   monitored_container_confirmed = Confirmed,
 			   options = Options,
 			   answerTo = StakeHolder}) ->


    Relation = #monitoring_relation{
		  monitoring_agent = Monitoring,
		  monitoring_container = node(),
		  prior_notification =  ?CREATEDNOTIFICATION,
		  monitored_agent = Monitored,
		  monitored_container = MonitoredContainer,
		  options = Options
		 },

    %% io:format("Generated relation: ~p~n",[Relation]),
    



    %% io:format("[SM] Monitored container: ~p~n",[MonitoredContainer]),
    case MonitoredContainer == node() of
	%% Monitoring and monitored are neighbours
	%% Then, just register the new relation (if it persists)
	true ->
	        
	    Persists = check_monitor_persistence(?CREATEDNOTIFICATION,
						 Options),
	    
	    NewInfo = 
		case Persists of
		    true ->
			case StakeHolder of 
			    %% The relation will be updated later
			    ?SM -> 
				Info;
			    _ ->

				%% io:format("4: NewRelation: ~p~n",[Relation]),
				add_monitoring_relation(Info, Relation)
			end;
		    false ->
			Info
		end,


	    NewTasks = dict:erase(TaskID,Tasks),
	    
	    %% Included to avoid sending the request/response when the 
	    %% stakeholder is self(). This occurs when the
	    %% register_monitor_request is sent after as result
	    %% of an agent creation
  
	    case StakeHolder of
		?SM ->
		    %% Send the delayed notification
		    process_monitoring_relation(
		      NewInfo#sm_info{tasks = NewTasks}, 
		      ?CREATEDNOTIFICATION, MonitoredContainer,
		      [{Monitoring, 
			Relation}]);
		_ ->
		    Response = 
			#monitor_response{id = TaskID,
					  monitored_agent = Monitored,
					  result = ?EJASONOK,
					  persistence = Persists
					 },
		    sm_send(StakeHolder,{TaskID,Response}),
		    NewInfo#sm_info{tasks = NewTasks}
	    end;	
	
	    

 	%% Monitored SM must by notified, as the monitoring relation 
	%% is duplicated
	%% It is duplicated because the SM-B is who ultimately decides whether
	%% the agent
	%% is restarted/revived/left to die... SM-B is who monitors B
	%% SM-A monitors SM-B to identify unreachable_agent events
	%% 
 	false ->
 	    

	    %% io:format("[SM] sending a monitor relation to monitored cont~n"),
	    case {Notified,Confirmed} of
 		{false,_} ->
 		    %% Notify SM-B if the relation shall persist
		    Persists = check_monitor_persistence(?CREATEDNOTIFICATION,
							 Options),

		    case Persists of 
			true ->
			    
			    Request = #register_monitor_request{
					 id = TaskID,
					 monitoring_agent = Monitoring,
					 monitored_agent = Monitored,
					 options = Options,
					 monitoring_container = node(),
					 prior_notification = ?CREATEDNOTIFICATION,
					 answerTo = node()},
			    
			    sm_send(MonitoredContainer, {TaskID,Request}),
		    
		    
		    %% Add local copy of the relation and monitor SM-B
			    NewMonitoredSM =
				case dict:find(MonitoredContainer, 
						  SMMonitored) of
				    {ok, _} ->
					%% SM-B is already monitored
					SMMonitored;
				    error ->
					%% Add a monitor to {SM, MonitoredCont}
					Ref =
					    erlang:monitor(process,
							   {?SM,
							    MonitoredContainer}),
					%% io:format("[SM] MONITORING SM at ~p~n"++
					%% 	 "MonitorTask: ~p~n",
					%% [MonitoredContainer,
					%% 	   MonitorTask]),
					dict:store(MonitoredContainer,
						       Ref,
						      SMMonitored)
				end,
			    
			    %% io:format("5: NewRelation: ~p~n",[Relation]),
			    %%add_monitoring_relation(Info,Relation),
			        
			    %% Do not add the relation now, wait for
			    %% SM-B to send the proper
			    %% monitor_notification.  Otherwise, the
			    %% value of prior_notification will be
			    %% wrong <- 
			    
				
			
			    
			    NewMonitorTask =
				MonitorTask#monitor_task{
				  monitored_container_notified = true},
			    
			    Info#sm_info{ 
			      monitored_sm = NewMonitoredSM,
			      tasks = dict:store(TaskID, NewMonitorTask, 
						    Tasks)};
			false ->
			    %% The monitor does not persist. Then do
			    %% not notify other containers.
			    Response = #monitor_response{id = TaskID,
							 monitored_agent = Monitored,
							 result = ?EJASONOK,
							 persistence = false},
			    
			    NewTasks = dict:erase(TaskID,Tasks),
			    sm_send(StakeHolder,{TaskID,Response}),
			    Info#sm_info{tasks = NewTasks}
		    end;
	       

		{true,true} ->
		    %% This step is only reached if the notification
		    %% persists after an agent creation. Otherwise, a
		    %% monitor_response is sent in the case-clause
		    %% above


		    %% Already notified
 		    NewTasks = dict:erase(TaskID,Tasks),
		    
		    
		    %% Included to avoid sending the response when the 
		    %% stakeholder is self(). This occurs when the
		    %% register_monitor_request is sent after as result
		    %% of an agent creation
		    case StakeHolder of
			?SM ->

			    %% NewInfo = add_monitoring_relation(Info,
			    %% 				      Relation),
			    %% Send the delayed notification, after
			    %% the reception of the
			    %% register_monitor_response
			    process_monitoring_relation(
			      Info#sm_info{tasks = NewTasks}, 
			      ?CREATEDNOTIFICATION, MonitoredContainer,
			      [{Monitoring, 
				Relation}]);
			_ ->

			    NewInfo = add_monitoring_relation(Info,
							       Relation),
			    
			    %% io:format("[SM] A NEW monitoring relation for a"++
			    %% 		  "distributed agent must be created."++
			    %% 		  "The one stored is: ~p~n",
			    %% 	      [check_exists_monitor(Info,
			    %% 				    Monitoring, 
			    %% 				    Monitored)]),

			    

			    Response = #monitor_response{id = TaskID,
							 monitored_agent = 
							     Monitored,
							 result = ?EJASONOK,
							 persistence = true},
			    
			    sm_send(StakeHolder,{TaskID,Response}),
			    NewInfo#sm_info{tasks = NewTasks}
				
		    end	
			    
	    end
    end;	    
	
%% DEMONITOR TASK (no more steps needed, just respond to the agent)
process_task(Info = #sm_info{tasks = Tasks},
	     #demonitor_task{id = TaskID,
			     monitoring_agent = Monitoring,
			     monitored_agent = Monitored,
			     %% monitored_container = MonitoredContainer,
			     monitored_container_notified = true,
			     monitored_container_confirmed = true,
			     %% monitor_retransfer_required = Required,
			     %% dm_retransfer_sent = Sent,
			     %% dm_retransfer_confirmed = Confirmed,
			     answerTo = StakeHolder}) ->

    %% when Required == false;
    %% 	   Sent == true,
    %% 	   Confirmed == true
    %% 	   ->
    
    Response =
	#demonitor_response{id = TaskID,
			    monitored_agent = Monitored,
			    result = ?EJASONOK},
    sm_send(StakeHolder,
	    {TaskID,Response}),

    
    %% Erase relation
    NewTasks =
	dict:erase(TaskID,
		      Tasks),  
    delete_monitoring_relation(Info#sm_info{tasks = NewTasks},
			       Monitoring,Monitored);


%% DEMONITOR TASK A notification is sent to the monitored container.
process_task(Info = #sm_info{tasks = Tasks},
	     DemonitorTask = 
	     	     
	     #demonitor_task{
	       id = TaskID,
	       monitored_agent = Monitored,
	       monitoring_agent = Monitoring,
	       monitored_container = MonitoredContainer,
	       monitored_container_notified = false,
	       monitored_container_confirmed = false})->
    
    %%send unregister_monitor request to SM-B
    
    UnregisterRequest =
	#unregister_monitor_request{id = TaskID,
				    answerTo = self(),
				    monitored_agent = Monitored,
				    monitoring_agent = Monitoring,
				    monitoring_container = node()},
    
    sm_send({?SM, MonitoredContainer},
	    {TaskID, UnregisterRequest}),
    
    %% Update task and return it to info
    
    NewTask =
	DemonitorTask#demonitor_task{ 
	  monitored_container_notified = true},
    NewTasks = dict:store(TaskID, NewTask, Tasks),
    
    Info#sm_info{tasks = NewTasks};

%%%%%%%%%%%%%%%%%%%%%%% SUPERVISION TASK

%% Some agent is already supervised, delete the task
%% Inform the supervisor agent
process_task(Info = #sm_info{tasks = Tasks},
	     #supervision_task{
		id = TaskID,
		result = ?EJASONERROR,
		supervisor_agent = Supervisor,
		supervised_set = SupSet,
		answerTo = StakeHolder})->
    Response = #supervision_response{
		  id = TaskID,
		  supervisor_agent = Supervisor,
		  supervised_set = SupSet,
		  result = ?EJASONERROR
		 },
    io:format("HERE2!~n"),
    sm_send(StakeHolder,{TaskID,Response}),
    NewTasks = 
	dict:erase(TaskID,
		      Tasks),

    Info#sm_info{tasks=NewTasks};

%% There are pending agents but no more SM to poll
%% Inform the supervisor agent of the failure
process_task(Info = #sm_info{tasks = Tasks},
	     #supervision_task{
		id = TaskID,
		result = ?EJASONERROR,
		supervisor_agent = Supervisor,
		supervised_set = SupSet,
		answerTo = StakeHolder})->
    Response = #supervision_response{
		  id = TaskID,
		  supervisor_agent = Supervisor,
		  supervised_set = SupSet,
		  result = ?EJASONERROR
		 },
    sm_send(StakeHolder,{TaskID,Response}),
    NewTasks = 
	dict:erase(TaskID,
		      Tasks),

    Info#sm_info{tasks=NewTasks};

%% Supervised agents pending
process_task(Info = #sm_info{tasks = Tasks},
	     #supervision_task{
		pending_agents = [_|_]})->
    Info;

%% No more supervised agents pending,
%% Create the supervision relations and inform the supervisor agent
process_task(Info = #sm_info{tasks = Tasks,
			     supervision_relations = Relations,
			     supervision_ancestors = InfoAncestors},
	     SupervisionTask = #supervision_task{
				  id = TaskID,
				  pending_agents = [],
				  supervisor_agent = Supervisor,
				  supervisor_container = Container,
				  supervised_set = SupSet,
				  supervision_policy = SupPolicy,
				  confirmed_agents = ConfirmedAgents,
				  answerTo = StakeHolder})->
    
    SupAncestors =
	case dict:find(Supervisor,InfoAncestors) of
	    error ->
		[]; %% No SupAgAncestors
	    {ok, Ancestors}->
		Ancestors
	end,
    

    
    SupervisionRelation =
	#supervision_relation{
	   supervisor_agent = Supervisor,
	   supervisor_container = Container,
	   supervised_set = SupSet,
	   supervision_policy = SupPolicy,
	   %% List of containers that maps the SupSet
	   supervised_set_containers = ConfirmedAgents,
	   ancestors = SupAncestors
	  },

    Request = #register_supervision_request{
		 id = TaskID,
		 supervisor_agent = Supervisor,
		 supervisor_container = node(),
		 supervision_relation = SupervisionRelation},
    

    %% io:format("ConfirmedAgents: ~p~n",[ConfirmedAgents]),
    %% Register relation in all other supervised SM (once per different container)
    SupervisedSetContainers =
	lists:delete(node(),
		     lists:usort(ConfirmedAgents)),

    %% io:format("[SM] SupervisedSetContainers: ~p~n",[SupervisedSetContainers]),

    lists:foreach(fun(X) ->
			  sm_send({?SM, X},
				  {TaskID, Request}) end,
		  SupervisedSetContainers),
    NewTasks = 
	dict:erase(TaskID,
		      Tasks),



    %% Notify the supervisor agent
    Response = #supervision_response{
		  id = TaskID,
		  supervisor_agent = Supervisor,
		  supervised_set = SupSet,
		  result = ?EJASONOK
		 },
    %%io:format("HERE3!~n"),

    sm_send(StakeHolder,{TaskID,Response}),	

    add_supervision_relation(Info#sm_info{
			       tasks = NewTasks}, SupervisionRelation);  

process_task(Info = #sm_info{},Task) ->
    io:format("[SM] ERROR Task: ~p~n",[Task]),
	exit(error_function_clause_process_task_in_SM).
		     



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CALL-BACK FUNCTIONS
%%
%% They start different supervision processes and return an ID for
%% the message that the invoking agent will expect to receive.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Procedure for agent A to monitor agent B:
%%
%%   1) A sends monitor_request to SM-A
%%   2) SM-A looks for B (find_agent_request)
%%   3) SM-A sends SM-B a register_monitor_request
%%      3.1) SM-A monitors SM-B, in case noconnection occurs
%%   4) SM-B sends SM-A a register_monitor_response
%%   5) SM-A sends A a monitor_request_response
%%
%%   In the end, is SM-B who monitors B and notifies SM-A of
%%   of possible restart/revival/death...

monitor(MonitoringAgent, MonitoredAgent, Options) ->

    MonitorRequest = 
	#monitor_request{ monitored_agent =MonitoredAgent,
			  monitoring_agent = MonitoringAgent,
			  answerTo = self(),
			  options = Options
			 },
    MonitorID = MonitorRequest#monitor_request.id,
    sm_send(?SM,{MonitorID,MonitorRequest}),
    MonitorID.







%% Procedure for agent A to demonitor agent B:
%%
%%   1) A sends demonitor_request to SM-A
%%   2) SM-A checks whether A monitors B
%%   3) SM-A sends SM-B an unregister_monitor_request
%%   4) SM-B sends SM-A an unregister_monitor_response
%%   5) SM-A sends A a demonitor_request_response
%%   6) A deletes its agent_up(A) and agent_down(A) beliefs

demonitor(MonitoringAgent, MonitoredAgent) ->

    
    DemonitorRequest = 
	#demonitor_request{
      monitored_agent =MonitoredAgent,
      monitoring_agent = MonitoringAgent,
      answerTo = self()
     },

    DemonitorID = DemonitorRequest#demonitor_request.id,

    sm_send({?SM, node()},{DemonitorID,DemonitorRequest}),

    DemonitorID.


%% Procedure for agent A to kill agent B:
%%
%%   1) A sends kill_agent_request to SM-A
%%   2) SM-A starts a find_agent task (via DM-A)
%%   3) DM-A informs SM-A about Pid-B
%%   4) SM-A sends a terminate_agent_request to SM-B
%%   5) SM-B kills Pid-B and carries out the proper 
%%      supervision actions
%%   6) SM-B sends a terminate_agent_response to SM-A (
%%      notification of the death)
%%   7) SM-A informs agent A

kill_agent(DyingAgent, KillingAgent)->
    KillAgentRequest =
	#kill_agent_request{
	   dying_agent_name = DyingAgent,
	   killing_agent_name = KillingAgent	  
	  },
    
    KillAgentID =
	KillAgentRequest#kill_agent_request.id,

    sm_send({?SM,node()}, {KillAgentID, KillAgentRequest}),
    
    KillAgentID.



%% Procedure for agent A to supervise agents in SupSet:
%%
%%   1) A sends supervision_request to SM-A
%%       1.1) SM checks that A is not trying to supervise an ancestor in its own supervision tree
%%   2) SM-A sends a supervise_agents_request to all known SM (including itself)
%%   3) Each SM checks its agents and, the ones that host some agent in the supervised set
%%      3.1) Check if that agent is already supervised if true -> send error
%%                                                    otherwise -> send OK
%%      3.2) Send a supervise_agent_response
%%   4) When all agents in the supervised set are ack'd send a register_supervision_request
%%   5) Each SM sends a register_supervision_response
%%

supervise(SupervisorAgent, SupervisedSet, SupervisionPolicy) ->
    SupervisionRequest = 
	#supervision_request{ supervisor_agent= SupervisorAgent,
			      supervised_set = SupervisedSet,
			      answerTo = self(),
			      supervision_policy = SupervisionPolicy
			    },
    SupervisionID = SupervisionRequest#supervision_request.id,
    %% io:format("[SM] Starting request: ~p~n",[SupervisionRequest]),
    sm_send(?SM,{SupervisionID,SupervisionRequest}),
    SupervisionID.

    
