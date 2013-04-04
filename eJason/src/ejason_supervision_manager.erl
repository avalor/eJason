-module(ejason_supervision_manager).

-export([
	 start/0, monitor/2,demonitor/2
	]).

-include("macros.hrl").
-include("sup_requests.hrl").
-include("suspendedActionsRecords.hrl").
-include("dm_requests.hrl").


-record(monitoring_relation, {
	monitoring_agent = no_monitoring,
	monitoring_container = no_monitoring_container,
	monitored_agent = no_monitored,
	monitored_container = no_monitored_container,
	reference = no_reference}).

-record(info, % supervision info
	{
	  monitoring_relations = orddict:new(), 
	  %{MonitoredAgent, {MonitoringAgent,#monitoring_relation}}  
	  % the shape is {key, dict}
	 % monitoring_agents = orddict:new(),
           %{MonitoringAgent, [MonitoredAgent]}
          % Used as a foreign-key convenience    
	  suspended_actions = orddict:new(), % TODO merge tasks and suspended_actions
	  tasks = orddict:new()
	 }). 




-record(task,
	{	  	  

	  type = missing_type, % is the same as that of request, but is duplicated for convenience
	  parent_task = is_parent,
	  
%	  task_info = no_task_info, % other info related to the task
	  pending_containers = no_pending_containers, %containers that have not answered yet  

%	  cumulative_agents =  no_cumulative_agents, % Used when request is of type  AGENTLIST 

	  %info = no_info,
%	  cumulative_containers =  no_cumulative_containers, %Cumulative containers for a CONNECT/CONNECTSYSTEM request
	  subtasks_ready = true,

	  updated = erlang:now(),

	  request = no_request% Request that started the task 
	 }).



start() ->
    register(?SM,self()),
    start(#info{}).

start(Info) when is_record(Info,info)->
    

    receive 
	Message ->
	    Message
    end,
    
%    io:format("~n~n [SM] PROCESSING MESSAGE: ~n~p~n",
%	      [Message]),
    
    LoopInfo =
	case Message of




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Monitor request (from agent to SM)

	    {?SM, MonitorReq = #sm_mon_request{type = ?MONITOR,
					       id = TS,
					       monitored_agent = Monitored}} ->
		case check_exists_monitor(Info,
					  MonitorReq) of
		    false -> % new relation
			FindAgentID = ?DM:find_agent(Monitored),
			ID = {?DM,FindAgentID},
			SuspendedAction = 
			    #suspended_sm_monitor{
			  request = MonitorReq },
			Info#info{ %% add suspended monitor
			  suspended_actions =
			           orddict:store(
				     ID,
				     SuspendedAction,
				     Info#info.suspended_actions)
			 };
		  

		    _ -> % Monitoring Relation already exists
			
			Response = #sm_mon_response{
			  type = ?MONITOR,
			  id = TS,
			  sentBy = node(),
			  result = ?EJASONOK
			 },
				
			sm_send(MonitorReq#sm_mon_request.answerTo,
				Response),
			Info
		end; %end of check_exists_monitor 
	     


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RegMonitor request (from SMa to SMb)

	    {?SM, RegMonReq = #sm_mon_request{type = ?REGMONITOR,
					     id = TS,
					     answerTo = MonitoringContainer,
					     monitoring_agent = Monitor,
					     monitored_agent = Monitored}} ->
			
		Reference =
		    erlang:monitor(process, Monitored),
		
		NewRelation = 
		    
		    #monitoring_relation{
		  monitoring_agent = Monitor,
		  monitoring_container = MonitoringContainer,
		  monitored_agent = Monitored,
		  monitored_container = node(),
		  reference = Reference
		 },
		
		RegMonTask =
		    #task{
		  type = ?REGMONITOR,
		  request = RegMonReq,
		  pending_containers = [],
		  subtasks_ready = false
		 },
		
		
		MonitoredRequest =
		    #sm_mon_request{
		  type = ?MONITORED,
		  answerTo = node(),
		  monitoring_agent = Monitor,
		  monitored_agent = Monitored				  
		 },

		sm_send(whereis(?DM), MonitoredRequest),
		
		MonitoredTask =
		    #task{
		  type = ?MONITORED,
		  request = MonitoredRequest,
		  pending_containers = [node()],
		  parent_task = TS,
		  subtasks_ready = true
		 },

		store_monitoring_relation(
		  Info#info{ % add new ?MONITORED and ?REGMONITOR tasks
						% add new monitoring_relation
		    tasks =
		    orddict:store(
		      TS,
		      RegMonTask,
		      orddict:store(
			MonitoredRequest#sm_mon_request.id,
			MonitoredTask,
			Info#info.tasks))
		   },
		  NewRelation);
	    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RegMonitor response (from SMb to SMa)

	    {?SM, #sm_mon_response{type = ?REGMONITOR,
				   id = TS,
				   sentBy = MonitoredContainer}} ->
		

		case orddict:find(TS,Info#info.tasks) of
		    {ok, #task{parent_task = ParentID}} ->
			case orddict:find(ParentID,Info#info.tasks) of
			    {ok, #task{
			       request = MonitorReq}} ->

				#sm_mon_request{
			      monitoring_agent = Monitor,
			      monitored_agent = Monitored
			     } = MonitorReq,
				
				Reference =
				    erlang:monitor(
				      process, 
				      {Monitored,
				       MonitoredContainer}),
				
				NewRelation = 
				    
				    #monitoring_relation{
				  monitoring_agent = Monitor,
				  monitoring_container = node(),
				  monitored_agent = Monitored,
				  monitored_container = MonitoredContainer,
				  reference = Reference
				 },  
				


		   
				MonitorResponse = #sm_mon_response{
				  type = ?MONITOR,
				  id = MonitorReq#sm_mon_request.id,
				  sentBy = node(),
				  result = ?EJASONOK
				 },
		
				sm_send(MonitorReq#sm_mon_request.answerTo,
					MonitorResponse),
		
				store_monitoring_relation(
				       % add new monitoring relation
				       %delete ?Monitor and ?RegMonitor tasks
				  Info#info{
				    tasks = orddict:erase(
					      MonitorReq#sm_mon_request.id,
					      orddict:erase(
						TS,
						Info#info.tasks))
				   },
				  NewRelation);
			    error -> % no_parent_task
				Info#info{%delete child task
				  tasks = orddict:erase(TS,
						Info#info.tasks)					
				 }
			end;
		    error -> %no regmonitor task
			Info
		end;
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DEMONITOR (agent to SM)
	    {?SM, #sm_demon_request{
		    id = TS,
		    answerTo = AnswerTo,
		    monitoring_agent = Monitoring,
		    monitored_agent = Monitored}} ->
		DemonResponse =
		    #sm_demon_response{
		  id = TS,
		  sentBy = node(),
		  result = ?EJASONOK
		 },

		NewInfo =
		    case check_exists_monitor(Info,Monitoring,
					      Monitored) of
			false -> % relation did not exist
			    Info;
			#monitoring_relation{
			      monitoring_container = Container,
			  reference = Reference
			 } ->
			erlang:demonitor(Reference),
			case Container of
			    _ when Container == node() -> %no need to notify
				ok;
			    _ ->
				EraseMonReq = #sm_erase_mon_request{
				  monitoring_agent = Monitoring,
				  monitored_agent = Monitored
				 },
				sm_send(Container,EraseMonReq)
			end,

			delete_monitoring_relation(Info,Monitoring,Monitored)
		    end,
		sm_send(AnswerTo,DemonResponse),
		NewInfo;

		

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ERASE MONITOR (SM to SM)
	    {?SM,  #sm_erase_mon_request{
		    monitoring_agent = Monitoring,
		    monitored_agent = Monitored}} ->
		
		delete_monitoring_relation(Info,Monitoring,Monitored);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% MESSAGES FROM ?DM
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FindAgent response (from DMb to SMb)

	    {?DM, Response =  #response{id = ID}} ->

		case  orddict:find({?DM,ID}, Info#info.suspended_actions) of
		    {ok, SuspendedInfo} ->
			NewSuspended = orddict:erase({?DM,ID},
						     Info#info.suspended_actions),
					
%		    io:format("SuspendedInfo: ~p~n",[SuspendedInfo]),
			process_dm_response(Info#info{
					      suspended_actions = NewSuspended},
					    Response,SuspendedInfo);
		    error-> %The suspended intention does not exist
		    
			%% %io:format("No suspended action with id: ~p~n",
			%% 	      [{?DM,ID}]),
			Info
		end;
		


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Monitored response (from DMb to SMb)

	    {?DM, #sm_mon_response{type = ?MONITORED,
				   id = TS}} ->
		
		case orddict:find(TS,Info#info.tasks) of
		    {ok, #task{parent_task = ParentID}} ->
			
			case orddict:find(ParentID, Info#info.tasks) of

                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Same container

			    {ok, #task{type = ?MONITOR,
				       request = MonitorReq}} ->
				
				MonitorResponse = #sm_mon_response{
				  type = ?MONITOR,
				  id = ParentID,
				  sentBy = node(),
				  result = ?EJASONOK
				 },
				
				sm_send(MonitorReq#sm_mon_request.answerTo,
					MonitorResponse),
				Info#info{ % remove ?monitor and ?monitored tasks
				  tasks =
				  orddict:erase(TS,
						orddict:erase( ParentID,
							       Info#info.tasks))
 				 };
                         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Different container

			    {ok, #task{type = ?REGMONITOR,
						   request = RegMonRequest}}->
				RegMonResponse = #sm_mon_response{
				  type = ?REGMONITOR,
				  id = ParentID,
				  sentBy = node(),
				  result = ?EJASONOK
				 },
				
				sm_send(RegMonRequest#sm_mon_request.answerTo,
					RegMonResponse),
				Info#info{ % remove ?monitor and ?monitored tasks
				  tasks =
				  orddict:erase(TS,
						orddict:erase( ParentID,
							       Info#info.tasks))
 				 };

                         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% outdated response
			   error -> % no_parent_task
				Info#info{%delete child task
				  tasks = orddict:erase(TS,
						Info#info.tasks)					
				 }
			end;
		    error -> %no monitored task
			Info
		end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Agent down  in the container

	    Down = {'DOWN',_Ref,process, {Monitored,_Node},_Reason} ->
		whereis(?DM) ! Message,
		%io:format("[SM] ~p has died.~n",[Monitored]),
		case orddict:find(Monitored,
				  Info#info.monitoring_relations) of
		    {ok,Monitors} ->
		%	io:format("Down is: ~p~nMonitors is: ~p~n",
		%		  [Down,Monitors]),
			send_agent_down(Down,Monitors);
		    
		    
		    error->
			ok
		end,
		
		Info;
	    
	    _ ->		
		io:format("Invalid SM Message: ~p~n",[Message]),
		Info
	end,
    start(LoopInfo).
    
								   

%% Auxiliary functions

% Checks whether some agent is already monitored
% Returns false or {true,Ref}
%is_monitored(#info{monitoring_relations = Monitors},AgentName)->
%    case orddict:find(AgentName,Monitors) of
%	error ->
%	    false;
%	{ok,Ref} ->
%	    {true,Ref} 
%   end.

% Checks if a monitoring relation already exists.
% Return false if it does not or the monitoring record otherwise. 

check_exists_monitor(Info,
		     #sm_mon_request{
		       monitoring_agent = Monitoring,
		       monitored_agent = Monitored
		      })->
    check_exists_monitor(Info,Monitoring,Monitored).


check_exists_monitor(#info{monitoring_relations = MRels},
		     Monitoring,Monitored)->
    
    case orddict:find(Monitored,
		      MRels) of
	error ->
	    false;
	{ok,[]} -> % monitoring relations where erased before
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
		    
% Add a new monitoring relation to the info record (two orddict:store needed)
store_monitoring_relation( Info = #info{ monitoring_relations = MRels},
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
        
    Info#info{% add new monitoring relation
      monitoring_relations = 
      orddict:store(
	Monitored,
	orddict:store(Monitoring,NewRelation,
		      OldMonitoringRels),
	MRels)
     }.


% Deletes a monitoring relation 
delete_monitoring_relation( Info = #info{ monitoring_relations = MRels},
			    Monitoring, Monitored) ->
   
    OldMonitoringRels = % add new relation for Monitored
	case orddict:find(Monitored,
			  MRels) of
	    error -> % no other agent monitors "Monitored"
		[];
	    {ok,Rels} ->
		Rels
	end,
        
    Info#info{% add new monitoring relation
      monitoring_relations = 
      orddict:store(
	Monitored,
	orddict:erase(Monitoring, OldMonitoringRels),
	MRels)
     }.





% Sends a message to some recipient appending the tag ?SM
% Several clauses depending on the recipient
sm_send(Pid, Message) when is_pid(Pid)-> %% Between a SM and an agent
    %io:format("~n[SM] Sending Message: ~p~n To: ~p~n~n",[Message,Pid]),
    Pid ! {?SM,Message};
sm_send(Container, Message) when is_atom(Container)-> % Between SMs
    %io:format("~n[SM] Sending Message: ~p~nTo: ~p~n~n",[Message,{?SM,Container}]),
    {?SM,Container} ! {?SM,Message};
sm_send(Other,_Message) ->
    io:format("[SM] Invalid Recipient: ~p~n",[Other]).


% Sends the agent_down message to all monitoring agents
send_agent_down(_Message,[])->
    ok;
send_agent_down(Message,[{Monitoring,#monitoring_relation{
			   monitoring_container = Container}}|Rest]) ->
    
    {Monitoring,Container} ! Message,
    send_agent_down(Message,Rest);
send_agent_down(Message,List) ->
    io:format("Bad sendagentdown: Message-> ~p~nList-> ~p~n",
	      [Message,List]),
    ok.




%%%%%%%%%%%%% PROCESS REQUESTS TO DM (reactivate actions)

process_dm_response(Info,Response = #response{type = ?FINDAGENT},
		    #suspended_sm_monitor{request = MonitorReq =
					  #sm_mon_request{type = ?MONITOR,
							  id = TS,
							  monitoring_agent = Monitor,
							  monitored_agent = Monitored}})->


    {_ID,InfoResponse,Result} = ?DM:process_response(Response),

   
    case Result of 
	?NOAGENT ->
	    %% monitored does not exist, then do not execute monitor but create relation
	    Reference = 
		erlang:monitor(process,Monitored),
		    	

	    NewRelation = 

		#monitoring_relation{
	      monitoring_agent = Monitor,
	      monitoring_container = node(),
	      monitored_agent = Monitored,
	      reference = Reference,
	      monitored_container = node()
	     },
						%TODO: add "disconnected_monitoring_relations"
	    store_monitoring_relation(Info, % Add new relation but no tasks
				      NewRelation);
	?AGENTFOUND ->
	    case InfoResponse of

		{Monitored,MonitoredNode,_Pid} 
		when MonitoredNode == node()-> % monitored in same container
		    Reference = 
			erlang:monitor(process,Monitored),
		    
		    NewRelation = 
			
			#monitoring_relation{
		      monitoring_agent = Monitor,
		      monitoring_container = 
		      node(),
		      monitored_agent = Monitored,
		      monitored_container = node(),
		      reference = Reference
		     },
		    
		    MonitoredRequest =
			#sm_mon_request{
		      type = ?MONITORED,
		      answerTo = node(),
		      monitoring_agent = Monitor,
		      monitored_agent = Monitored		    
		     },
		    
		    sm_send(whereis(?DM),
			    MonitoredRequest),
		    
		    MonitoredTask =
			#task{
		      type = ?MONITORED,
		      parent_task = TS,
		      pending_containers = [],
		      request = MonitoredRequest
		     },

		    MonitorTask =
			#task{
		      type = ?MONITOR,
		      pending_containers = [],
		      subtasks_ready = false,
		      request = MonitorReq
		     },

		    store_monitoring_relation(%add monitoring rel
		      Info#info{ % adding monitor and monitored tasks
			tasks =
			orddict:store(
			  MonitoredRequest#sm_mon_request.id,
			  MonitoredTask,
			  orddict:store(
			    TS,
			    MonitorTask,
			    Info#info.tasks))
		       },
		      NewRelation);




		{Monitored,MonitoredContainer,_Pid} -> % Monitored exist in another container
						%TODO optimize when MonitoredContainer == node()
		    RegMonRequest =
			#sm_mon_request{
		      type = ?REGMONITOR,
		      answerTo = node(),
		      monitoring_agent = Monitor,
		      monitored_agent = Monitored				  
		     },

		    sm_send(MonitoredContainer,
			    RegMonRequest),

		    RegMonitorTask =
			#task{
		      type = ?REGMONITOR,
		      parent_task = TS,
		      pending_containers = [MonitoredContainer],
		      request = RegMonRequest
		     },

		    MonitorTask =
			#task{
		      type = ?MONITOR,
		      pending_containers = [],
		      subtasks_ready = false,
		      request = MonitorReq
		     },

		    Info#info{ % add new ?MONITOR and ?REGMONITOR tasks
		      tasks =
		      orddict:store(
			TS,
			MonitorTask,
			orddict:store(
			  RegMonRequest#sm_mon_request.id,
			  RegMonitorTask,
			  Info#info.tasks))
		     }
	    end % end of case(InfoRequest)
    end; % end of case(Result)
process_dm_response(Info, Response,SuspendedInfo) ->
    io:format("[SM] Incorrect response:~nInfo: ~p~nResponse: ~p~nSuspendedInfo: ~p~n",
	      [Info,Response,SuspendedInfo]),
    Info.








%% CALLBACK FUNCTIONS

monitor(MonitoringAgent, MonitoredAgent) ->
    MonitorRequest = 
	#sm_mon_request{
      type = ?MONITOR,
      monitored_agent =MonitoredAgent,
      monitoring_agent = MonitoringAgent,
      answerTo = self()
     },
    sm_send(node(),MonitorRequest),
    MonitorID = MonitorRequest#sm_mon_request.id,
    receive {?SM,
	     #sm_mon_response{id = MonitorID,
			      result = Result}}->
	    
	    case Result of
		?EJASONOK ->
		    {?STUTTERACTION};
		_ ->
		    {?FAIL}
	    end
	    
    after 15000 ->
	    {?FAIL}
    end.

demonitor(MonitoringAgent, MonitoredAgent) ->
    DemonitorRequest = 
	#sm_demon_request{
      monitored_agent =MonitoredAgent,
      monitoring_agent = MonitoringAgent,
      answerTo = self()
     },

    sm_send(node(),DemonitorRequest),
    MonitorID = DemonitorRequest#sm_mon_request.id,
    receive {?SM,
	     #sm_demon_response{id = MonitorID,
			      result = Result}}->
	    
	    case Result of
		?EJASONOK ->
		    {?STUTTERACTION};
		_ ->
		    {?FAIL}
	    end
	    
    after 15000 ->
	    io:format("Timedout demonitor Request~n"),
	    {?FAIL}
    end.


