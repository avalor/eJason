-module(ejason_supervision_manager).

-export([start/0, monitor/3,
	 demonitor/2, kill_agent/2]).

-include("include/macros.hrl").
-include("include/sm_requests.hrl").
-include("include/sm_responses.hrl").
-include("include/sm_tasks.hrl").
-include("include/records.hrl"). %% contains monitor_options
-include("include/dm_sm_requests.hrl").
-include("include/dm_sm_responses.hrl").


-record(sm_info, % supervision info
	{
	  monitoring_relations = orddict:new(),
	  %% The key are the names of the monitored agents 

	  monitored_agents = orddict:new(), 
	  %% All agents in the container are monitored by the SM.


	  %% The corresponding restart strategy is applied when they die.
	  %% The SM monitor must inform the DM about deaths


	  %% monitoring_agents = orddict:new(),
	  %% {MonitoringAgent, [MonitoredAgent]}
          %% Used as a foreign-key convenience    
	  %% tasks = orddict:new(), 

	  tasks = orddict:new(),

	  %% These tasks are indexed by the name of the supervised agent.
	  %% Each key/value may contain several tasks (e.g. killing an agent
	  %% requested by two different sources-> the agent dies twice if it
	  %% revived in-between).
	  %% When they require other tasks like, find_agent, the task is 
	  %% duplicated within the field tasks.
	  actions = orddict:new()

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
%% TODO: do not rely on PIDs as stakeholders
sm_send(?DM, Message) -> %% Send a message to local ?DM
    {?DM,node()} ! {?SM,Message};
sm_send(?SM, Message) -> %% Send a message to local ?SM
    {?SM,node()} ! {?SM,Message};
sm_send(Pid, Message) when is_pid(Pid)-> %% Between a SM and an agent
    %io:format("~n[SM] Sending Message: ~p~n To: ~p~n~n",[Message,Pid]),
    Pid ! {?SM,Message};
%% TODO: this case should be deleted eventually. "Dangerous"
sm_send(Container, Message) when is_atom(Container)-> % Between SMs
    %io:format("~n[SM] Sending Message: ~p~nTo: ~p~n~n",[Message,{?SM,Container}]),
    {?SM,Container} ! {?SM,Message};
sm_send({?SM, Container}, Message) when is_atom(Container)-> % Between SMs
    {?SM,Container} ! {?SM,Message};

sm_send(Other,Message) ->
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
    %% 	      [Message]),
    
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
		
		
		MonitorTask = 
		    #monitor_task{id = RequestID,
				  answerTo = StakeHolder,
				  monitored_agent = Monitored,
				  monitoring_agent = Monitoring,
				  options = Options
				 },

		case check_exists_monitor(Info, Monitoring, Monitored) of
		    false -> % new relation
			FindAgentID = ?DM:find_agent(Monitored),

			%% TODO: avoid possible race condition if the ?SM
			%% received a 
			%% duplicated request for the same agent.
			%% Reason: monitor task is indexed under FindAgentID

			NewInfo =    Info#sm_info{ %% add task
				       tasks =
				       orddict:store(
					 FindAgentID,
					 MonitorTask,
					 InfoTasks)},
			process_task(NewInfo,MonitorTask);
		    
		    #monitoring_relation{
				      monitored_container = 
					  MonitoredContainer} -> 
			%% Monitoring Relation already exists
		

			%% Update relation with the new configuration
			NewTask =
			    MonitorTask#monitor_task{
			      agent_found_result = ?AGENTFOUND,
			      monitored_container = MonitoredContainer,
			      monitored_container_notified = false,
			      monitored_container_confirmed = false},
			
			process_task(Info,NewTask)


		end; %end of check_exists_monitor 
	     

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% REGISTER MONITOR request (from SMa to SMb)

	    {?SM, {RequestID,  #register_monitor_request{
		     answerTo = StakeHolder,
		     monitoring_agent = Monitoring,
		     monitored_agent = Monitored,
		     options = Options,
		     monitoring_container = MonitoringContainer}}} ->
		
		%% The monitoring relation is replicated in the SM at monitored 
		%% container.
		
		%% <- erlang:monitor(process, Monitored),
		
		NewRelation = 
		    #monitoring_relation{
		       monitoring_agent = Monitoring,
		       monitoring_container = MonitoringContainer,
		       monitored_agent = Monitored,
		       monitored_container = node(),
		       prior_notification = ?UNKNOWN,
		       options = Options
		      },
		
		NewInfo = add_monitoring_relation(Info,NewRelation),
		
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
		
		
		case orddict:find(ResponseID,InfoTasks) of
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
		
		sm_send(MonitoringContainer, {RequestID, Response}),
		
		NewInfo;
	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UNREGISTER MONITOR response (SMb to SMa)

	    {?SM, {ResponseID,
		   #unregister_monitor_response{
		     id = ResponseID,
		     result = ?EJASONOK}}} ->
		
		
		case orddict:find(ResponseID,InfoTasks) of
		    {ok, DemonitorTask = #demonitor_task{}}->

			RetransferRequired = false,
			    %% retransfer_after_demonitor(InfoMonitors,Monitoring,
			    %% 			       Monitored),

			NewTask = DemonitorTask#demonitor_task{
				    monitored_container_confirmed = true,
				    monitor_retransfer_required = 
				    RetransferRequired},
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


		NewTask =
		    case check_exists_monitor(Info,Monitoring,
					      Monitored) of
			false -> % relation did not exist, no more steps
			    
			    DemonitorTask#demonitor_task{
			      monitored_container = node(),
			      monitored_container_notified = true,
			      monitored_container_confirmed = true,
			      monitor_retransfer_required = false, 
			      dm_retransfer_sent = false, 
			      dm_retransfer_confirmed = false};
		    
			
			%%Monitoring relation exists
			#monitoring_relation{monitoring_container = 
					     MonitoringContainer,
					     monitored_container =
					     MonitoredContainer
					    } ->
			    

			    %% erlang:demonitor(Reference),
			    case MonitoringContainer of
				MonitoredContainer -> 
				    TransferRequired = false,
					%% retransfer_after_demonitor(InfoMonitors,
					%% 			   Monitoring,
					%% 			   Monitored),

				    %% == node(), no need to notify
				    DemonitorTask#demonitor_task{
				      monitored_container = node(),
				      monitored_container_notified = true,
				      monitored_container_confirmed = true,

				      monitor_retransfer_required = 
				      TransferRequired, 
				      dm_retransfer_sent = false, 
				      dm_retransfer_confirmed = false};
				    
				_ ->
				   
				    %% Demonitor must be sent to SM-B
				    DemonitorTask#demonitor_task{
				      monitored_container = MonitoredContainer,
				      monitored_container_notified = false,
				      monitored_container_confirmed = false,
				      monitor_retransfer_required = false,
				      dm_retransfer_sent = false, 
				      dm_retransfer_confirmed = false}
				      
			    end
		    end,

		process_task(Info,NewTask);
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% NOTIFY MONITOR REQUEST (from SMb to SMa)
	    {?SM, {RequestID, #monitor_notification_request{
				 id = RequestID,
				 answerTo = StakeHolder,
				 monitoring_agent = Monitoring,
				 monitored_agent = Monitored,
				 notification = Change,
				 relation_persist = Persists}}} ->

		

		OldRelation = 
		    check_exists_monitor(Info,Monitoring,Monitored),
		NewInfo =
		    case OldRelation of
			#monitoring_relation{
			   prior_notification = Prior}->
			    
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
					   monitored_agent = Monitored,
					   notification = Change,
					   prior_notification = Prior},
				    
				    sm_send(Pid,Notification)
			    end,
			    
			    
			    case Persists of
				true ->
				    
				    NewRelation = 
					case Change of 
					    %% Monitored container no longer
					    %% known
					    _ when ?DEAD== Change orelse 
						   Change==?UNREACHABLE ->
						%% Delete monitored container
						OldRelation#monitoring_relation{
						  monitored_container = 
						      ?NOMONITOREDCONTAINER,
						  prior_notification = Change};
					    _ ->
						OldRelation#monitoring_relation{
						  prior_notification = Change}
					end,
				    %% Modify 
				    %% the prior notification 
				    %% of the monitoring relation
				    add_monitoring_relation(Info, 
							    NewRelation);
				
				false ->
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
	    {?SM, {ResponseID, #monitor_notification_response{
		     id = ResponseID,
		     monitoring_agent = Monitoring,
		     monitored_agent = Monitored,
		     notification = Change}}} ->

		case orddict:find(ResponseID,InfoTasks) of
		    {ok, NotifyMonitorTask = #notify_monitor_task{}}->

			%% delete task
			NewTasks = orddict:erase(ResponseID, InfoTasks),
			
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
		    orddict:append(DyingAgent, KillAgentRequest,
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
			    orddict:update(DyingAgent,
					   fun (List) ->
						   [TerminateAgentRequest|List]
					   end,
					   TerminateAgentRequest,
					   InfoActions);
			_ ->
			    orddict:append(DyingAgent, TerminateAgentRequest,
					   InfoActions)
		    end,
		%% Will execute the action if this is the only one queued
		execute_supervision_actions(Info#sm_info{actions= NewActions},
					    DyingAgent);
		  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TERMINATE AGENT response (from SMb to SMa)
	    {?SM, {ResponseID,  
		   TerminateAgentResponse =
		       #terminate_agent_response{}}} ->
		
		NewTasks = orddict:erase(ResponseID, InfoTasks),
		NewInfo = Info#sm_info{tasks = NewTasks},

		case orddict:find(ResponseID, InfoTasks) of
		    
		    %%% Send the response to the killing agent
		    {ok, KillTask = #kill_agent_task{ id = ResponseID}} ->
			process_action(NewInfo, TerminateAgentResponse,
				       KillTask);
		    error ->
			NewInfo
		end;
	    
		

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% MESSAGES FROM ?DM
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% START MONITOR request (DMa to SMa, at creation)
	    {?DM, {RequestID,
		   #start_monitor_request{
		     id = RequestID,
		     monitored_agent = Monitored}}} ->	
		
		NewRef = 
		    erlang:monitor(process,{Monitored, node()}),
		
		NewInfo = 
		    Info#sm_info{ %update the monitor
		      monitored_agents =
			  orddict:store(Monitored,
					NewRef,
					Info#sm_info.monitored_agents)
		     },
		
		Response =
		    #start_monitor_response{
		  monitored_agent = Monitored,
		  id = RequestID,
		  result = ?EJASONOK
		 },

		sm_send(?DM,{RequestID,Response}),
	       
		
		NewInfo;
	   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Agent Creation Notification (local DM to SM)
	    
	    {?DM, {_NotificationID, #agent_creation_notification{
				       container = Container,
				       agent_name = CreatedAgentName }}}->
		
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

		case orddict:find(ResponseID,InfoTasks) of
		    
		    {ok, #erase_agent_task{ id = ResponseID}}->
		     %% Erase agent task, delete it
			NewInfo = 
			    Info#sm_info{
			      tasks = orddict:erase(ResponseID,InfoTasks)
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
		    
		case  orddict:find(ResponseID, InfoTasks) of
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
			    orddict:erase(ResponseID, InfoTasks),
			NewInfo =
			    Info#sm_info{tasks = NewTasks},
			%% orddict:erase(ResponseID,
			%% 		   NewTasks)},
			
			process_task(NewInfo,NewTask);
			{ok, KillAgentTask =#kill_agent_task{}} ->
			NewTasks =
			    orddict:erase(ResponseID, InfoTasks),
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% monitor notifications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


	    %% An agent has died in the container

	    _Down = {'DOWN',_Ref,process, {Monitored,_Node},Reason} 
	      when Monitored =/= ?SM->
	
		%% io:format("[SM] Ha muerto: ~p~n",[Down]),
		%% TODO: Apply supervision policy here, 
		%% then may change FailReason (revive, restart)
		process_down_message(Info, Monitored, Reason);

	    %% The SM of a monitored agent is unreachable/dead, then it 
	    %% will not send notifications, the node is assumed to be
	    %% disconnected 
	    _Down = {'DOWN',_Ref,process, {?SM,Node},Reason}->
	
		%% io:format("[SM] Ha muerto: ~p~n",[Down]),
		%% TODO: Apply supervision policy here, 
		%% then may change FailReason (revive, restart)
		process_down_message(Info, {?SM, Node}, Reason);



	    _ ->		
		io:format("[SM DEBUG:] Invalid SM Message: ~p~n",[Message]),
		Info
	end,
    start(LoopInfo).
    
								   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% AUXILIARY FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Invokes 'process_monitoring_relation' for every relation involving Monitored
%% New #notify_monitor tasks for each notification.
%% Some relation may be deleted (if they do not persist the change)
%% Returns a new #sm_info
process_monitors(Info = #sm_info{
		   monitoring_relations = InfoMRels},
		 Monitored, MonitoredContainer, Change)->
    

    case orddict:find(Monitored,
		      InfoMRels) of
	
	error -> % the agent is not monitored  
	    %% io:format("[SM] The agent ~p is not monitored. No notification~n",
	    %% 	      [Monitored]),
	    %% io:format("[SM] Monitoring relations: ~p ~n",[InfoMRels]),
	    Info;
	{ok,Rels} ->
	    %% io:format("[SM] notifying Change: ~p~n to relations: ~p~n",
	    %% 	      [Change, Rels]),
	    
	    List = orddict:to_list(Rels),	    
	    process_monitoring_relation(Info,Change,MonitoredContainer,List)
    end.




%% Sends the proper notifications to the monitoring agents
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
    NewTasks = orddict:store(NewMonitorTask#monitor_task.id,
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
    
    TaskID = erlang:now(),
    
    
    %% TODO: maybe avoid this step when MonitoringContainer = MonitoredContainer
    NotificationRequest = 
	#monitor_notification_request{ id = TaskID,
				       answerTo = node(),
				       monitoring_agent = Monitoring,
				       monitored_agent = Monitored,
				       notification = Change,
				       relation_persist = Persists},
    
    sm_send({?SM, MonitoringContainer},{TaskID,NotificationRequest}),
    
    NewTask = #notify_monitor_task{ id = TaskID,
				    monitored_agent = Monitored,
				    monitoring_agent = Monitoring,
				    monitoring_container = MonitoringContainer,
				    notification = Change,
				    relation_persist = Persists},
    
    NewTasks = orddict:store(TaskID,NewTask,InfoTasks),


    
    NewInfo = 
	%% Do not change anything if SMa = SMb, as it will be done
	%% upon reception of the monitor_notification_request.
	case MonitoringContainer == node()  of 
	    true ->
	       	Info#sm_info{tasks=NewTasks};
	    %% Update the monitoring relation, as SMa will do
	    false ->
		case Persists of
		    true ->
	  
			NewRelation = 
			    case Change of 
				_ when ?DEAD== Change orelse 
				       Change==?UNREACHABLE ->
				    %% Delete monitored container
				    Relation#monitoring_relation{
				      monitored_container = 
					  ?NOMONITOREDCONTAINER,
				      prior_notification = Change};
				_ ->
				    Relation#monitoring_relation{
				      prior_notification = Change}
			    end,
			%% Add the new task and modify the prior notification 
			%% of the monitoring relation
			add_monitoring_relation(Info#sm_info{ 
						  tasks = NewTasks}, 
						NewRelation);
		  
		    false ->
			%% delete the monitoring relation and add the new task
			delete_monitoring_relation(Info#sm_info{tasks=NewTasks},
						   Monitoring,Monitored)
		end
	end,
    process_monitoring_relation(NewInfo,Change,MonitoredContainer, Rest).

   


%% Checks if a monitoring relation already exists.
%% Return 'false', if it does not, or the monitoring record, otherwise. 
check_exists_monitor(#sm_info{monitoring_relations = MRels},
		     Monitoring, Monitored)->
    
    case orddict:find(Monitored,
		      MRels) of
	error ->
	    false;
	{ok,[]} -> %all  monitoring relations where erased before
	    false; 
	{ok, MonitoringDict} -> % dict of monitors of the monitored agent
	    case orddict:find(Monitoring,
			      MonitoringDict) of
		error ->
		    false;
		
		{ok, Relation} -> %relation already exists
		    Relation
	    end
    end.
		    
%% Adds a new  monitoring relation (or updates an existing one)
%% to the info record (two orddict:store needed)
add_monitoring_relation( Info = #sm_info{ monitoring_relations = MRels},
			  NewRelation = #monitoring_relation{
			   monitoring_agent = Monitoring,
			   monitored_agent = Monitored}) ->

    %io:format("Relation stored: ~p~n",[NewRelation]),
    
    OldMonitoringRels = % add new relation for Monitored
	case orddict:find(Monitored,
			  MRels) of
	    error -> % no other agent monitors "Monitored"  
		[];
	    {ok,Rels} ->
		Rels
	end,
        
    Info#sm_info{% add new monitoring relation
      monitoring_relations =   
      orddict:store(
	Monitored,
	orddict:store(Monitoring,NewRelation,
		      OldMonitoringRels),
	MRels)
     }.


%% Deletes a monitoring relation
%% Returns an sm_info record
delete_monitoring_relation( Info = #sm_info{ monitoring_relations = MRels},
			    Monitoring, Monitored) ->
   
    OldMonitoringRels = 
	case orddict:find(Monitored,
			  MRels) of
	    error -> % no other agent monitors "Monitored"
		[];
	    {ok,Rels} ->
		Rels
	end,
        
    Info#sm_info{
      monitoring_relations = case OldMonitoringRels of
				 [] ->
				     %% Erase monitored
				     orddict:erase(Monitored,MRels);
				 _ ->
				     %% Erase only one relation
				     orddict:store(
				       Monitored,
				       orddict:erase(Monitoring,
						     OldMonitoringRels),
				       MRels)
			     end}.
%% Returns a list of the monitoring relations such that MonitoredCont
%% is the container where the monitore agent runs
get_relations_for_monitored_container(MRels, MonitoredCont)->
    %% [{Monitored, [Relations]}]
    List =
	orddict:to_list(
	  orddict:map(fun (_Key, Orddict) ->
			      %% [{Monitoring, MRelation}]
			      ListMonitoring  = orddict:to_list(Orddict),
			      lists:filtermap(
				fun({_Monitoring, 
				    Relation =
					#monitoring_relation{
					   monitored_container =
					       MonitoredCont}})->
					{true, Relation};
				   (_) ->
					false
				end,
				ListMonitoring)
		      end,
		      MRels)),
    %% [[Relations1], [Relations2]...]
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
%% 	case orddict:find(Monitored,
%% 			  MRels) of
%% 	    error -> % The relation does not exist
%% 		[];
%% 	    {ok,Rels} ->
%% 		Rels
%% 	end,
    
%%     case orddict:find(OldMonitoringRels,Monitoring) of
%% 	[] ->
%% 	    [];
%% 	{ok,Relation} ->
		
%% 	    Relation
%%     end.




%% Checks whether a monitoring relation persists after Change
check_monitor_persistence(Change, #monitor_options{
			    persist_unknown_agent = Unknown,
			    persist_dead_agent = Dead,
			    persist_restarted_agent = Restarted,
			    persist_revived_agent = Revived,
			    persist_unreachable_agent = Unreachable,
			    persist_created_agent = Created})->
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




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Down Messages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% Returns an updated sm_info record


%% Another SM is dead. Notify the monitoring agents that
%% monitor some agent is SM's container.
process_down_message(Info= #sm_info{ monitoring_relations = MRels},
		     {?SM, MonitoredCont}, 
		     Reason)->
    {_ActInfo, FailReason}=
	case Reason of
	    noconnection ->
		{Info, ?UNREACHABLE};
	    _ ->
		{Info, ?DEAD}
	end,
    Relations =
	get_relations_for_monitored_container(MRels, MonitoredCont),

    Fun = fun (#monitoring_relation{
		  monitored_agent = Monitored},
	       SMInfo) ->
		  process_monitors(SMInfo,Monitored, MonitoredCont, FailReason)
	  end,
    lists:foldl(Fun, Info, Relations);
  
process_down_message(Info= #sm_info{tasks = InfoTasks,
				  actions = InfoActions},
		     Monitored, Reason)->
    {ActInfo, FailReason}=
	case Reason of
	    noproc ->
		{Info, ?UNKNOWN};
	    noconnection ->
		{Info, ?UNREACHABLE};
	    %% {?SM, Reason, aa} ->
	    %%     {InfoActions, ?RESTART};
	    %% Then SM killed it. Process the related
	    %% actions. 
	    {?SM, ActionID}->
		UpdatedInfo =
		    case orddict:find(Monitored, InfoActions) of
			{ok, [Action|Actions]}->
			    process_action(Info, 
					   {'DOWN', Monitored, ActionID},
					   Action);
			error ->
			    %% Outdated agent_down message
			Info
		    end,
		{UpdatedInfo, ?DEAD};
	    _ ->
		{Info, ?DEAD}
	end,
    
    %% Temporal: no supervision yet%%%%%%%%%-> Delete agent
    
    MonInfo =
	process_monitors(ActInfo,Monitored,node(),FailReason),
    
    TaskID = erlang:now(),
    
    NewTask = 
	#erase_agent_task{ id = TaskID,
			   dead_agent = Monitored},

    Request =
	#erase_agent_request{ id = TaskID,
			      dead_agent = Monitored},

    sm_send(?DM,{TaskID,Request}),

    NewTasks = orddict:store(TaskID,
			     NewTask,
			     InfoTasks),
    NewInfo = MonInfo#sm_info{
		%% add new erase_agent_task
		tasks = NewTasks},     


    NewInfo.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SUPERVISION RELATED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Executes a supervision action for AgentName (e.g. kill, restart...)
%% if the first queued action is a request. If it is a task, it is
%% already being carried out, so nothing changes. 
execute_supervision_actions(Info =
				#sm_info{actions= InfoActions},
			    AgentName)->
    
    QueuedActions =
	case orddict:find(AgentName, InfoActions) of
	    {ok, List} ->
		List;
	    error ->
		[]
	end,
    
    
    case QueuedActions of
	[] ->
	    
	    %% All queued actions have been executed
	    NewActions = orddict:erase(AgentName, InfoActions),
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
			     killing_agent_name = KillingAgent}|
			  RestActions]) when DyingContainer == node()->
    
    Action = 
	#terminate_agent_action{
	   id = RequestID,
	   dying_agent_name = DyingAgent,
	   killing_agent_name = KillingAgent,
	   killing_container = KillingContainer},
    
    NewActions = orddict:store(
		   DyingAgent, 
		   [Action|RestActions], InfoActions),
        
    case whereis(DyingAgent) of
	Pid when is_pid(Pid) ->
	    %% The agent is alive
	    %% NewTasks = 
	    %% 	orddict:store(RequestID,
	    %% 		      Task,
	    %% 		      InfoTasks),
	    
	    %% Kill the agent
	    erlang:exit(Pid, {?SM, RequestID}),
	    Info#sm_info{actions = NewActions};
	_ ->
	    %% The agent no longer exists here.
	    %% Answer back as if a down message were received.
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
	orddict:store(FindAgentID,
		      KillAgentTask,
		      InfoTasks),

    NewActions = orddict:store(AgentName,
			       [KillAgentTask|RestActions],
			       InfoActions),

    Info#sm_info{ %% add task and replace action
      tasks = NewTasks,
      actions = NewActions}.
    




%% This function processes the responses related to the different
%% supervision actions. It returns an updated sm_info 

%% A down message is received as part of a terminate_agent action.
process_action(Info= #sm_info{tasks = InfoTasks,
			      actions = InfoActions},
	       #terminate_agent_response{
		  id = ResponseID,
		  result = Result},
	       KillTask = #kill_agent_task{
			     id = ResponseID,
			     dying_agent_name = DyingAgent,
			     killing_agent_name = KillingAgent})->
    case orddict:find(DyingAgent, InfoActions) of 
	{ok, [KillAgentTask|RestActions]} ->
	    
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
	    NewActions = orddict:store(DyingAgent, RestActions,
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
	       TerminateAction = #terminate_agent_action{
				    id = TerminateID,
				    dying_agent_name = DyingAgent,
				    killing_agent_name = KillingAgent,
				    killing_container = KillingContainer})->
    %% io:format("[SM] InfoActions: ~p~n",[InfoActions]),

    {ok, [TerminateAction|RestActions]}= orddict:find(
					   DyingAgent,
					   InfoActions),
    
    
    Response = #terminate_agent_response{
		  id = TerminateID,
		  dying_agent_name = DyingAgent,
		  killing_agent_name = KillingAgent},
    
    
    sm_send({?SM, KillingContainer}, {TerminateID, Response}),
    
    NewActions = orddict:store(DyingAgent, RestActions, 
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
	    %%    actions = orddict:erase(DyingAgent, InfoActions)
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
		   killing_agent_name = KillingAgent		       
		  },
	    
	    sm_send({?SM, Container}, {KillID, TerminateRequest}),
	    
	    NewTasks = orddict:store(KillID, NewKillTask, InfoTasks),
	    Info#sm_info{
	      tasks = NewTasks
	      }
    end.
	    
	    
	    


%% %% Must decide whether the agent dies for good or not, before it
%% %% allows the killing agent to continue. This way, if this agent
%% %% invokes a .connect() where would be a clash with DyingAgent,
%% %% the clash will not occur.

%% 		MonitorTask#monitor_task{
%% 		  agent_found_result = Result,
%% 		  monitored_container = Container}
%%     end,
    
%%     NewTasks =
%% 	orddict:erase(ResponseID, InfoTasks),
%%     NewInfo =
%% 	Info#sm_info{tasks = NewTasks},
%%     %% orddict:erase(ResponseID,
%% 			%% 		   NewTasks)},


	       







%% %% Updates a monitoring relation (e.g. to change the value of prior_notification)
%% %% Returns an sm_info record
%% update_monitoring_relation( Info = #sm_info{ monitoring_relations = MRels},
%% 			    UpdatedRelation = #monitoring_relation{
%% 			      monitoring_agent=Monitoring,
%% 			      monitored_agent = Monitored}) ->
   
%%     OldMonitoringRels = % add new relation for Monitored
%% 	case orddict:find(Monitored,
%% 			  MRels) of
%% 	    error -> % no other agent monitors "Monitored"
%% 		[];
%% 	    {ok,Rels} ->
%% 		Rels
%% 	end,
        
%%     Info#sm_info{
%%       monitoring_relations = case OldMonitoringRels of
%% 				 [] ->
%% 				     %% Erase monitored
%% 				     orddict:erase(Monitored,MRels);
%% 				 _ ->
%% 				     %% Erase only one relation
%% 				     orddict:store(
%% 				       Monitored,
%% 				       orddict:erase(Monitoring,
%% 						     OldMonitoringRels),
%% 				       MRels)
%% 			     end}.






%% % Sends the agent_down message to all monitoring agents
%% send_agent_down(_Message,[])->
%%     ok;
%% send_agent_down(Message,[{Monitoring,#monitoring_relation{
%% 			   monitoring_container = Container}}|Rest]) ->
    
%%     {Monitoring,Container} ! Message,
%%     send_agent_down(Message,Rest);
%% send_agent_down(Message,List) ->
%%     io:format("Bad sendagentdown: Message-> ~p~nList-> ~p~n",
%% 	      [Message,List]),
%%     ok.




%% %%%%%%%%%%%%% PROCESS REQUESTS TO DM (reactivate actions)

%% process_dm_response(Info,Response = #response{type = ?FINDAGENT},
%% 		    #suspended_sm_monitor{request = 
%% 					  (MonitorReq =
%% 					  #sm_mon_request{
%% 					    type =MONITOR,
%% 					    id = TS,
%% 					    monitoring_agent = Monitor,
%% 					    monitored_agent = Monitored})})->
    

%%     {_ID,InfoResponse,Result} = ?DM:process_response(Response),

   
%%     case Result of 
%% 	?NOAGENT ->
%% 	    %% monitored does not exist, then do not execute monitor but create relation
%% 	    Reference = 
%% 		erlang:monitor(process,Monitored),
		    	

%% 	    NewRelation = 

%% 		#monitoring_relation{
%% 	      monitoring_agent = Monitor,
%% 	      monitoring_container = node(),
%% 	      monitored_agent = Monitored,
%% 	      reference = Reference,
%% 	      monitored_container = node()
%% 	     },
%% 						%TODO: add "disconnected_monitoring_relations"
%% 	    store_monitoring_relation(Info, % Add new relation but no tasks
%% 				      NewRelation);
%% 	?AGENTFOUND ->
%% 	    case InfoResponse of

%% 		{Monitored,MonitoredNode,_Pid} 
%% 		when MonitoredNode == node()-> % monitored in same container
%% 		    Reference = 
%% 			erlang:monitor(process,Monitored),
		    
%% 		    NewRelation = 
			
%% 			#monitoring_relation{monitoring_agent = Monitor,
%% 					     monitoring_container =  node(),
%% 					     monitored_agent = Monitored,
%% 					     monitored_container = node(),
%% 					     reference = Reference
%% 					    },
		    
%% 		    MonitoredRequest =
%% 			#sm_mon_request{
%% 		      type = ?MONITORED,
%% 		      answerTo = node(),
%% 		      monitoring_agent = Monitor,
%% 		      monitored_agent = Monitored		    
%% 		     },
		    
%% 		    sm_send(whereis(?DM),
%% 			    MonitoredRequest),
		    
%% 		    MonitoredTask =
%% 			#task{
%% 		      type = ?MONITORED,
%% 		      parent_task = TS,
%% 		      pending_containers = [],
%% 		      request = MonitoredRequest
%% 		     },

%% 		    MonitorTask =
%% 			#task{
%% 		      type = ?MONITOR,
%% 		      pending_containers = [],
%% 		      subtasks_ready = false,
%% 		      request = MonitorReq
%% 		     },

%% 		    store_monitoring_relation(%add monitoring rel
%% 		      Info#sm_info{ % adding monitor and monitored tasks
%% 			tasks =
%% 			orddict:store(
%% 			  MonitoredRequest#sm_mon_request.id,
%% 			  MonitoredTask,
%% 			  orddict:store(
%% 			    TS,
%% 			    MonitorTask,
%% 			    Info#sm_info.tasks))
%% 		       },
%% 		      NewRelation);




%% 		{Monitored,MonitoredContainer,_Pid} -> % Monitored exist in another container
%% 		    %%TODO optimize when MonitoredContainer == node()
%% 		    RegMonRequest =
%% 			#sm_mon_request{
%% 		      type = ?REGMONITOR,
%% 		      answerTo = node(),
%% 		      monitoring_agent = Monitor,
%% 		      monitored_agent = Monitored				  
%% 		     },

%% 		    sm_send(MonitoredContainer,
%% 			    RegMonRequest),

%% 		    RegMonitorTask =
%% 			#task{
%% 		      type = ?REGMONITOR,
%% 		      parent_task = TS,
%% 		      pending_containers = [MonitoredContainer],
%% 		      request = RegMonRequest
%% 		     },

%% 		    MonitorTask =
%% 			#task{
%% 		      type = ?MONITOR,
%% 		      pending_containers = [],
%% 		      subtasks_ready = false,
%% 		      request = MonitorReq
%% 		     },

%% 		    Info#sm_info{ % add new ?MONITOR and ?REGMONITOR tasks
%% 		      tasks =
%% 		      orddict:store(
%% 			TS,
%% 			MonitorTask,
%% 			orddict:store(
%% 			  RegMonRequest#sm_mon_request.id,
%% 			  RegMonitorTask,
%% 			  Info#sm_info.tasks))
%% 		     }
%% 	    end % end of case(InfoRequest)
%%     end; % end of case(Result)
%% process_dm_response(Info, Response,SuspendedInfo) ->
%%     io:format("[SM] Incorrect response:~nInfo: ~p~nResponse: ~p~nSuspendedInfo: ~p~n",
%% 	      [Info,Response,SuspendedInfo]),
%%     Info.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Process Task (after updates), returns a new #sm_info
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% MONITOR TASK (monitored not yet found)
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
    
    %% 1) Add the new relation (prior notification is set to "unknown")

    NewRelation = #monitoring_relation{
		     monitoring_agent = Monitoring,
		     monitoring_container = node(),
		     monitored_agent = Monitored,
		     prior_notification = ?UNKNOWN,
		     monitored_container = ?NOMONITOREDCONTAINER,
		     options = Options
		    },
	    
    NewInfo = add_monitoring_relation(Info,NewRelation),

    %% 2) Send the response
    
    Response = 
	#monitor_response{ id = TaskID,
			   monitored_agent = Monitored,
			   result = ?NOAGENT},
    sm_send(StakeHolder,
	    {TaskID,Response}),
    
    NewTasks =
	orddict:erase(TaskID,
		      Tasks),

   
    NewInfo#sm_info{tasks = NewTasks};

%% MONITOR TASK (monitored found)
%% Monitored SM must be notified
process_task(Info = #sm_info{tasks = Tasks},
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
		     prior_notification = ?UNKNOWN,
		     monitored_agent = Monitored,
		     monitored_container = MonitoredContainer,
		     options = Options
		    },

    %% io:format("[SM] Monitored container: ~p~n",[MonitoredContainer]),
    case MonitoredContainer == node() of
	%% Monitoring and monitored are neighbours
	%% Then, just register the new relation
	true ->
	        
	    
	    NewInfo = add_monitoring_relation(Info,Relation),
	    NewTasks = orddict:erase(TaskID,Tasks),
	    
	    Response = 
		#monitor_response{id = TaskID,
				  monitored_agent = Monitored,
				  result = ?EJASONOK},

	    %% Included to avoid sending the response when the 
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
 		    %% Notify SM-B 
 		    Request = #register_monitor_request{
				 id = TaskID,
				 monitoring_agent = Monitoring,
				 monitored_agent = Monitored,
				 options = Options,
				 monitoring_container = node(),
				 answerTo = node()},
		    
		    sm_send(MonitoredContainer, {TaskID,Request}),
		    
		    
		    %% Add local copy of the relation and monitor SM-B

		    
		    erlang:monitor(process,{?SM,MonitoredContainer}),
		    
		    NewInfo = add_monitoring_relation(Info,Relation),


		    NewMonitorTask =
			MonitorTask#monitor_task{
			  monitored_container_notified = true},

		    NewInfo#sm_info{ 
		      tasks = orddict:store(TaskID, NewMonitorTask, Tasks)};

		{true,true} ->
		    
		    %% Already notified
 		    NewTasks = orddict:erase(TaskID,Tasks),
		    
 		    Response = #monitor_response{id = TaskID,
 						 monitored_agent = Monitored,
 						 result = ?EJASONOK},
		    
		    %% Included to avoid sending the response when the 
		    %% stakeholder is self(). This occurs when the
		    %% register_monitor_request is sent after as result
		    %% of an agent creation
		    case StakeHolder of
			?SM ->
			    %% Send the delayed notification
			    process_monitoring_relation(
			      Info#sm_info{tasks = NewTasks}, 
			      ?CREATEDNOTIFICATION, MonitoredContainer,
			      [{Monitoring, 
				Relation}]);
			_ ->
			    sm_send(StakeHolder,{TaskID,Response}),
			    Info#sm_info{tasks = NewTasks}
				
		    end	
			    
	    end
    end;	    
process_task(Info = #sm_info{},Task) ->
    io:format("[SM] ERROR Task: ~p~n",[Task]),
	exit(error_function_clause_process_task_in_SM).
	
%% %% DEMONITOR TASK (no more steps needed)
%% process_task(Info = #sm_info{tasks = Tasks},
%% 	     #demonitor_task{id = TaskID,
%% 			     monitoring_agent = Monitoring,
%% 			     monitored_agent = Monitored,
%% 			     %% monitored_container = MonitoredContainer,
%% 			     monitored_container_notified = true,
%% 			     monitored_container_confirmed = true,
%% 			     monitor_retransfer_required = Required,
%% 			     dm_retransfer_sent = Sent,
%% 			     dm_retransfer_confirmed = Confirmed,

%% 			     answerTo = StakeHolder}) when Required == false;
%% 							   Sent == true,
%% 							   Confirmed == true
%% 							   ->
    
%%     Response =
%% 	#demonitor_response{id = TaskID,
%% 			    monitored_agent = Monitored,
%% 			    result = ?EJASONOK},
%%     sm_send(StakeHolder,
%% 	    {TaskID,Response}),

    
%%     %% Erase relation
%%     NewTasks =
%% 	orddict:erase(TaskID,
%% 		      Tasks),
    
%%     delete_monitoring_relation(Info#sm_info{tasks = NewTasks},
%% 			       Monitoring,Monitored);
%% %% DEMONITOR TASK (== node(), no need to notify, retransfer required)
%% process_task(Info = #sm_info{tasks = Tasks},
%% 	     DemonitorTask = 
	     	     
%% 	     #demonitor_task{
%% 	       id = TaskID,
%% 	       monitored_agent = Monitored,
%% 	       monitored_container_notified = true,
%% 	       monitored_container_confirmed = true,
	       
%% 	       monitor_retransfer_required = true, %% Retransfer to DM 
%% 	       dm_retransfer_sent = false, 
%% 	       dm_retransfer_confirmed = false})->
    
%%     RetransferRequest =
%% 	#retransfer_monitor_request{ id = TaskID,
%% 				     answerTo = self(),
%% 				     monitored_agent = Monitored},
%%     %%send retransfer request
    
%%     sm_send(whereis(?DM),{TaskID, RetransferRequest}),


%%     %% Update task and add it

%%     NewTask =
%% 	DemonitorTask#demonitor_task{ dm_retransfer_sent = true},
    
%%     NewTasks = orddict:store(TaskID,NewTask,Tasks),

%%     Info#sm_info{tasks = NewTasks};

%% %% DEMONITOR TASK (MonitoringCont =/= MonitoredCont,  needs to notify)
%% process_task(Info = #sm_info{tasks = Tasks},
%% 	     DemonitorTask = 
	     	     
%% 	     #demonitor_task{
%% 	       id = TaskID,
%% 	       monitored_agent = Monitored,
%% 	       monitoring_agent = Monitoring,
%% 	       monitored_container = MonitoredContainer,
%% 	       monitored_container_notified = false,
%% 	       monitored_container_confirmed = false})->
    
%%     %%send unregister_monitor request to SM-B
    
%%     UnregisterRequest =
%% 	#unregister_monitor_request{id = TaskID,
%% 				    answerTo = self(),
%% 				    monitored_agent = Monitored,
%% 				    monitoring_agent = Monitoring,
%% 				    monitoring_container = node()},

    
%%     sm_send(MonitoredContainer,{TaskID, UnregisterRequest}),


%%     %% Update task and add it

%%     NewTask =
%% 	DemonitorTask#demonitor_task{ monitored_container_notified = true},
    
%%     NewTasks = orddict:store(TaskID,NewTask,Tasks),

%%     Info#sm_info{tasks = NewTasks}.

    %% receive {?SM,
    %% 	     #sm_demon_response{id = MonitorID,
    %% 			      result = Result}}->
	    
    %% 	    case Result of
    %% 		?EJASONOK ->
    %% 		    {?STUTTERACTION};
    %% 		_ ->
    %% 		    {?FAIL}
    %% 	    end
	    
    %% after 15000 ->
    %% 	    io:format("Timedout demonitor Request~n"),
    %% 	    {?FAIL}
    %% end.


				     
      

	     
	    
  




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
%%   6) SM-B informs SM-A of the death
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




    
