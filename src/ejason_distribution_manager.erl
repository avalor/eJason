-module(ejason_distribution_manager).

-export([start/0,
	 start/1,
	 send_message/2,
	 name_agent/2,
	 create_agent/3,
	 find_agent/1,
	 connect/1,
	 disconnect/1,
	 get_info/0, get_info/1,
	 print_info/0]).

-include("include/macros.hrl").
-include("include/dm_requests.hrl").
-include("include/dm_responses.hrl").	     
-include("include/dm_tasks.hrl").

-include("include/dm_sm_requests.hrl").
-include("include/dm_sm_responses.hrl").	     


%% NOTE: The functionality of Erlang's "global" module differs from the one
%%      implemented by the DM of eJason. e.g. Merging two MAS where a name clash
%%      occurs does not unregister one of these agents, but informs the DM
%%      so the conflict can be solved programmatically.


-record(dm_info, % dm_info
	{agents = ordsets:new(),% Registered agents
	 reserved_names = ordsets:new(), % Agent names not resolved yet
	 containers = ordsets:new(),% Connected containers
	 cachec = orddict:new(), % Name cache [{Name,Container}]
	 %%monitored_agents = orddict:new(), 
	 %% These agents are not monitored by the SM, therefore not monitored or
	 %% supervised by another agent. Are erased when their process dies.


	 tasks= orddict:new()
	}).    % tasks pending to proceed (name polls, connections...)




%% Sends a message to some recipient appending the tag ?DM
%% Several clauses depending on the recipient
%%
%% TODO: change logics. Relying on Pids implies losing requests 
%%       for revived agents (e.g. rely on names, but check if they are dead)

dm_send(?DM, Message) -> %% Send a message to local ?DM
    {?DM,node()} ! {?DM,Message};
dm_send(?SM, Message) -> %% Send a message to local ?SM
    {?SM,node()} ! {?DM,Message};
dm_send(Pid, Message) when is_pid(Pid)-> %% Between a DM and an agent
  %%    io:format("~nSending Message: ~p~n To agent: ~p~n~n",[Message,Pid]),
    Pid ! {?DM,Message};
dm_send({?SM, Container}, Message) when is_atom(Container)-> % To some SM
    {?SM,Container} ! {?DM,Message};
dm_send({?DM, Container}, Message) when is_atom(Container)-> % Between DMs
  %%   io:format("~nSending Message: ~p~nTo: ~p~n~n",[Message,{?DM,Container}]),
    {?DM,Container} ! {?DM,Message};

dm_send(Other,Message) ->
    io:format("[DM]Invalid Recipient: ~p~nMessage: ~p~n",[Other,Message]).

  


start()->
    register(?DM,self()),
    ?DM:start(#dm_info{}).

start(Info = #dm_info{containers = InfoContainers}) ->
    % TODO: consider using a gen_server
	receive % Returns a new Info record
	    Message ->
		Message
	end,

  
     %% io:format("~n~n [DM] PROCESSING MESSAGE at ~p:~n~p~n",
     %% 	      [node(),Message]),
  
   %%  io:format("Info is: ~p~n",[Info]),
    LoopInfo =
	case Message of
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Send Message request (received by DMa from agent)

	    {?DM, {RequestID, 		   
		   #send_message_request{ 
		     id = RequestID,
		     answerTo = StakeHolder,
		     agent_name = AgentName,
		     suggested_container = SuggestedContainer}
		   %% performative=Performative,
		   %% message = Message}
		  }} when is_pid(StakeHolder)->
		
		
		%% find the recipient agent
		SendMessageTask =
		    case check_name(Info,RequestID,AgentName) of 
			?NAMECLASH  ->
			    %% agent found in node
			    
			    FindAgentTask =
				#find_agent_task{id = RequestID,
						 answerTo = StakeHolder,
						 agent_name = AgentName,
						 pending_containers = [],
						 found_in = node()},
			    
			    #send_message_task{id = RequestID,
					       %% answerTo = StakeHolder,
					       agent_name = AgentName,
					       suggested_container = 
						   SuggestedContainer,
					       find_agent = FindAgentTask,
					       connection_attempted = true};
			?NOAGENT -> 
			    %% No more containers to look into
			    
			    FindAgentTask =
				#find_agent_task{id = RequestID,
						 answerTo = StakeHolder,
						 agent_name = AgentName,
						 pending_containers = []},
			    %% io:format("FindAgentTask: ~p~n",[FindAgentTask]),
			    
			    Send = #send_message_task{
				      id = RequestID,
				      %% answerTo = StakeHolder,
				      agent_name = AgentName,
				      suggested_container = 
					  SuggestedContainer,
				      find_agent = FindAgentTask},
			    %% io:format("Send: ~p~n",[Send]),
			    Send;
			
			FindAgentTask = #find_agent_task{}->
			    %% try to find the agent
			    #send_message_task{id = RequestID,
					       agent_name = AgentName,
					       suggested_container =
					       SuggestedContainer,
					       find_agent = 
					       FindAgentTask#find_agent_task{
						 answerTo = StakeHolder}}

			end,
		
		
		NewTasks =
		    orddict:store(RequestID,
				  SendMessageTask,
				  Info#dm_info.tasks),
		
		NewInfo =
		    Info#dm_info{tasks = NewTasks},
		
		%%io:format("SendMessageTask: ~p~n",[SendMessageTask]),

		process_task(NewInfo,RequestID,SendMessageTask);
	       


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Create Agent request (received by DMa from agent)

	    {?DM, {RequestID, 
		   Request=
		   #create_agent_request{
		     answerTo = StakeHolder,
		     agent_name = AgentName,
		     container = Container,
		     code = Code}
		  }
	    } when is_pid(StakeHolder)->
		
		if Container == node() ->
			%% The new agent will be created in this container
			case check_name(Info,RequestID,AgentName) of 
			    ?NAMECLASH  -> % name clash, respond now
				Response =
				    #create_agent_response{ 
				  id = RequestID,
				  agent_name = AgentName,
				  container = Container,
				  code = Code,
				  result = ?NAMECLASH
				 },				
				dm_send(StakeHolder,
					{RequestID,Response}),
				Info;
			    
			    ?NOAGENT -> % No more containers
			%% io:format("[DM] No more containers. Creating: ~p~n", [AgentName]),
				NewTask =
				    #create_agent_task{ 
				  id = RequestID,
				  request = Request,
				  answerTo = StakeHolder,
				  %%will be as soon as create_agent is invoked
				  sm_notified = true,
				  find_agent = 
				  #find_agent_task{pending_containers = [],
						   found_in = no_found_in,
						   answerTo = StakeHolder}},
				NewInfo =
				    Info#dm_info {
				      %% 1)reserve the name  
				      reserved_names = 
				      ordsets:add_element(
					AgentName,
					Info#dm_info.reserved_names),
				      %% 2) add the new task
				      tasks =
				      orddict:store(
					RequestID,
					NewTask,
					Info#dm_info.tasks)},
				
				create_agent(NewInfo,Request);
			    %%% There are more containers, the name must be checked in them
			    SubTask = #find_agent_task{}->
				NewTask =
				    #create_agent_task{ id = RequestID,
							request = Request,
							answerTo = StakeHolder,
							find_agent = 
							SubTask#find_agent_task{
							  answerTo = 
							  StakeHolder}
						       },
				
%%% no clash, store task while is_agent responses arrive
				Info#dm_info{ 
				  %% 1)reserve the name			  
				  reserved_names = 
				  ordsets:add_element(
				    AgentName,
				    Info#dm_info.reserved_names),
				  %% 2) add the new task
				  tasks =
				  orddict:store(
				    RequestID,
				    NewTask,
				    Info#dm_info.tasks)}
			end;
		   
		   true ->
			%% The request must be forwarded, if Container exists
			case lists:member(Container, Info#dm_info.containers) of
			    true ->
				%% forward
				dm_send({?DM, Container},{RequestID,Request}),
				Info;
			    false ->
				%% container does not exist
				Response =
				    #create_agent_response{ 
				  id = RequestID,
				  agent_name = AgentName,
				  container = Container,
				  code = Code,
				  result = ?NOCONTAINER
				 },				
				dm_send(Request#create_agent_request.answerTo,
					{RequestID,Response}),
				Info
			end
		end;
	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Agent creation notification (from DMa)
	    
	    {?DM, {NotificationID, Notification = #agent_creation_notification{}}}->
		%% just bounce the notification to ?SM
		dm_send(?SM,{NotificationID,Notification}),
		Info;
	    


			       

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Find agent request (from agent to DMa)
	    {?DM, {RequestID,
		   #find_agent_request{agent_name = AgentName,
				       id = RequestID,
				       answerTo = StakeHolder}}} 
	    when is_pid(StakeHolder)->
		case check_name(Info,RequestID,AgentName) of 
		    ?NAMECLASH  -> % name clash, respond now
			Response =
			    #find_agent_response{ id = RequestID,
						  result = ?AGENTFOUND,
						  agent_name = AgentName,
						  container = node(),
						  agent_pid = whereis(AgentName)
						 },				
			dm_send(StakeHolder,
				{RequestID,Response}),
			Info;
		    
		    ?NOAGENT-> % No answer from other containers needed
			Response =
			    #find_agent_response{ id = RequestID,
						  result = ?NOAGENT,
						  agent_name = AgentName,
						  container = node()
						 },				
			dm_send(StakeHolder,
				{RequestID,Response}),			
			Info;
		    NewTask = #find_agent_task{}->
			%% no clash, store task while responses arrive
			Info#dm_info{ %add new find_agent_task 
			  tasks =
			  orddict:store(
			    RequestID,
			    %% answerTo and agent_name fields are copied
			    NewTask#find_agent_task{
			      answerTo = StakeHolder,
			      agent_name = AgentName},
			    Info#dm_info.tasks)
			 }
		end;
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Is agent request
	    {?DM, {RequestID,
		   Request = #is_agent_request{agent_name = AgentName,
					       answerTo = StakeHolder}}}->
		
		%% Is agent request can only be processed if the StakeHolder is
		%% a PID (some agent) or if it is a connected container.
		%% Otherwise, these type of requests are witnesses for some failure
		Condition =
		    is_pid(StakeHolder) or 
		    lists:member(StakeHolder,
				 InfoContainers),

		if
		    Condition == true ->
			Receiver = case  StakeHolder of
				       _ when is_pid(StakeHolder)->
					   StakeHolder;
				       _ ->
					   {?DM, StakeHolder}
				   end,
			case ordsets:is_element(AgentName,Info#dm_info.agents) 
			    orelse
			    ordsets:is_element(AgentName,
					       Info#dm_info.reserved_names)
			    of
			    false ->
				Response =
				    #is_agent_response{ id = RequestID,
							result = ?NOAGENT,
							agent_name = AgentName
						       },				
				dm_send(Receiver,
					{RequestID,Response}),
				Info;
			    true->
				Response =
				    #is_agent_response{ id = RequestID,
							result = ?AGENTFOUND,
							agent_pid = whereis(AgentName),								
							agent_name = AgentName
						       },				
				dm_send(Receiver,
					{RequestID,Response}),
				Info
			    
			end;
		    true ->
			io:format("[SM] Stakeholder: ~p  Is agent Request: ~p~n"
				 ,[StakeHolder,Request]),
			Info
		end;
				    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Is agent response 
	    {?DM, {ResponseID,
		   #is_agent_response{
		     agent_pid = AgentPid,
		     result = Result,
		     sent_by = Container}}}   ->
				  
		case orddict:find(ResponseID,Info#dm_info.tasks) of
		    error ->
			Info; % message out of date. Ignored 
		    {ok,Task} ->
			%% Recall that is_agent queries belong to a find_agent
			%% task (which can belong to e.g. a create_agent task)
			FindAgentTask =
			    case Task of
				#find_agent_task{} ->
				    Task;
				#create_agent_task{} ->
				    Task#create_agent_task.find_agent;
				#send_message_task{} ->
				    Task#send_message_task.find_agent
			    end,
		
			%% Update find_agent_task
			NewFindAgentTask =
			    case Result of
				?NOAGENT ->
				    %% One pending container less
				    PendingContainers =
					FindAgentTask#find_agent_task.pending_containers,
				    NewPending =
					lists:delete(Container,
						     PendingContainers),
				    
				    FindAgentTask#find_agent_task{
				      pending_containers = NewPending};
				?AGENTFOUND ->
				    %% Agent found, find_agent_task "completed" 
				    
				    FindAgentTask#find_agent_task{
				      pending_containers = [],
				      found_in = Container,
				      agent_pid = AgentPid
				     }
			    end,
			
			%% Continue processing task/parent task

			case Task of
			    #find_agent_task{} ->
				process_task(Info,ResponseID,NewFindAgentTask);
			    #create_agent_task{} ->
				process_task(Info,
					     ResponseID,
					     Task#create_agent_task{
					       find_agent =NewFindAgentTask});
			    #send_message_task{} ->
				process_task(Info,
					     ResponseID,
					     Task#send_message_task{
					       find_agent =NewFindAgentTask})   
			end
		end;
	      
			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Connect response (subtask of a send_message)
	    {?DM, {ResponseID,
		   #connection_response{
		     id = ResponseID,
		     result = Result,
		     parent_task = ParentID}}}   ->
				  
		case orddict:find(ResponseID,Info#dm_info.tasks) of
		    error ->
			Info; % message out of date. Ignored 
		    {ok,_} ->
			{ok,ParentTask} = orddict:find(ParentID,
						       Info#dm_info.tasks),
			%% Modify the send_message task after the connection
			
			FindAgentTask =ParentTask#send_message_task.find_agent,
			StakeHolder = FindAgentTask#find_agent_task.answerTo,
			
			NewParentTask =
			    case Result of
				?CONNECTED ->
				    %% Look for the agent again
				    case check_name(Info,ParentID,ParentTask#send_message_task.agent_name) of 
					?NAMECLASH  ->
					    %% agent found in node
					    ParentTask#send_message_task{
					      find_agent = 
					      FindAgentTask#find_agent_task{
						pending_containers = [],
						found_in = node()}};
					?NOAGENT -> 
					    %% No more containers to look into
					    ParentTask#send_message_task{
					      find_agent = 
					      FindAgentTask#find_agent_task{
						pending_containers = []}};
					
					NewFindAgentTask = 
					#find_agent_task{}->
					    %% try to find the agent
					    ParentTask#send_message_task{
					      find_agent = 
					      NewFindAgentTask#find_agent_task{
						answerTo = StakeHolder}}
				    end;
				_ -> %%?NAMECLASH; ?NOCONTAINER
				    ParentTask#send_message_task{
				      find_agent = 
				      FindAgentTask#find_agent_task{
					pending_containers = []}}
			    end,
			NewInfo =
			    Info#dm_info{tasks = 
					 orddict:store(ParentID,
						       ParentTask,
						       orddict:erase(ResponseID,
								     Info#dm_info.tasks))},
			process_task(NewInfo,ParentID,NewParentTask)
		end;
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Connect request (from agent to DMa)
	     {?DM, {RequestID,
		    #connection_request{
		      container = Container,
		      answerTo = StakeHolder,
		      parent_task = ParentID
		     }}} when is_pid(StakeHolder)->
		    %%io:format("Request: ~p~n",[Request]),
	
		case connect_container(Info,RequestID,Container) of
		    ?CONNECTED -> % already connected
			Response =
			    #connection_response{id = RequestID,
						 sent_by = node(),
						 result = ?CONNECTED,
						 parent_task = ParentID
					     },
			%%Request = Task#task.request,
			dm_send(StakeHolder,
				{RequestID,Response}),
			%%io:format("Connected already~n"),
			Info;
		    ?NOCONTAINER -> %% the container does not exist
			Response =
			    #connection_response{id = RequestID,
						 sent_by = node(),
						 result = ?NOCONTAINER,
						 parent_task = ParentID
						},
			dm_send(StakeHolder,
				{RequestID,Response}),
			Info;


		    AgentListRequest = #agent_list_request{}->
			%% Agent Names being gathered

			%% MyAgents = ordsets:union(Info#dm_info.agents,
			%% 			 Info#dm_info.reserved_names), 
			InitiatorTask =
			    #system_connection_initiator_task{
			       id = RequestID,
			       initiator_agent = StakeHolder,
			       container = Container,
			       agent_list_request = AgentListRequest,
			       pending_agent_list_containers =
				   Info#dm_info.containers,
			       %% My agents will be added later on
			       gathered_agents = orddict:new(),
				   %% orddict:store(node(), MyAgents,
				   %% 		 orddict:new()),
			       

			       pending_connect_to_containers =
				   Info#dm_info.containers		       
			 },

			%% io:format("InitiatorTask: ~p~n",[InitiatorTask]),

			process_task(Info,RequestID,InitiatorTask)
		end;
	          


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Disconnect request (from agent to DMa)
	%%TODO: StakeHolder MUST be pid of one of the agents in this container
	    {?DM, {RequestID,
		   #disconnection_request{
		     id = RequestID,
		     answerTo = StakeHolder,
		     container = Container}}} when is_pid(StakeHolder)->

		%% Question =
		%%     ordsets:is_element(Container,
		%% 		       Info#dm_info.containers) or
		%%     (Container == self()),
		%% io:format("Question: ~p~n",[Question]),
		
		{NewTask,NewContainers} =
		    case ordsets:is_element(Container,
					    Info#dm_info.containers) or
			(Container == self()) of
			false ->
			    %% container already out of the MAS
			    {   #system_disconnection_task{
			  id = RequestID,
			  answerTo = StakeHolder,
			  container = Container,
			  pending_containers = []},
				Info#dm_info.containers};
			
			true ->   	    
	   
			    %% tell rest of containers to detach from node()
			    MyRequest = #system_disconnection_request{
			      id = RequestID,
			      answerTo = node(),
			      container = node()},
			    
			    lists:map(fun(OtherContainer) ->
					      dm_send({?DM,OtherContainer},
						      {RequestID,
						       MyRequest})
				      end,
				      Info#dm_info.containers),

			       

			    %% new task, new containers = []
			    {#system_disconnection_task{
				       id = RequestID,
				       answerTo = StakeHolder,
				       container = Container,
				       pending_containers = 
				       Info#dm_info.containers},
			     []}
		    end,
		
		NewInfo =
		    Info#dm_info{tasks = orddict:store(RequestID,
						       NewTask,
						       Info#dm_info.tasks),
				containers = NewContainers},
		
		process_task(NewInfo,RequestID,NewTask);
	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% System disconnection request (from DM to DM)
	    {?DM, {RequestID,
		   #system_disconnection_request{id = RequestID,
						 answerTo = StakeHolder,
						 container = Container}}} ->
		%% Send response

		case lists:member(StakeHolder,
				  InfoContainers)  of
		    true ->
			
			
			erlang:disconnect_node(Container);
		    false ->
			%% Container already disconnected
			ok			
		end,	

		Response = #system_disconnection_response{
		  id = RequestID,
		  sent_by = node()},
		
		dm_send({?DM, StakeHolder},
			{RequestID,
			 Response}),
		
		Info#dm_info{
		  containers = ordsets:del_element(
				 Container,
				 InfoContainers)};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% System disconnection response (from DM to DM)
	    {?DM, {ResponseID,
		   #system_disconnection_response{
		     id = ResponseID,
		     sent_by = Sender}}} ->
	    
		case orddict:find(ResponseID,Info#dm_info.tasks) of
		    error ->
			Info; % message out of date. Ignored 
		    {ok, Task = #system_disconnection_task{
			   id = ResponseID,
			   pending_containers = Pending}}->
			
			NewPending = ordsets:del_element(Sender,Pending),
			NewTask =
			    Task#system_disconnection_task{
			      pending_containers = NewPending},
			
		    
			NewTasks =
			 orddict:store(ResponseID,
				       NewTask,
				       Info#dm_info.tasks),
		     
			NewInfo =
			    Info#dm_info{tasks = NewTasks},

			process_task(NewInfo,ResponseID,NewTask)

		end;
	             

	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% System connection request (from Initiator to Receiver)
	      {?DM, {RequestID,
		     #system_connection_request{id = RequestID,
						answerTo = Initiator}}} ->
		
		%% TODO: what if Initiator is already considered connected??

		%% send check_agents request
		
		GatherRequest = gather_agents(Info,RequestID),
		
		%% send check_agents_request to DMa
		
		CheckRequest= #check_agents_request{
		  id = RequestID,
		  answerTo = node()
		 },
		
		dm_send({?DM, Initiator},{RequestID,CheckRequest}),
		
		
		
		%% Create a new receiver task
		
		%% MyAgents = ordsets:union(
		%% 	     Info#dm_info.agents,
		%% 	     Info#dm_info.reserved_names),      
		    
		ConnectTask =
		    #system_connection_receiver_task{
		  id = RequestID,
		  container = Initiator,
		  agent_list_request = GatherRequest,
		  check_agents_response_received = false,
		  pending_agent_list_containers = Info#dm_info.containers,
		  gathered_agents = orddict:new()
		 },

		%% io:format("ConnectTask ~p~n",[ConnectTask]),

		NewTasks =
		    orddict:store(RequestID,
				  ConnectTask,
				  Info#dm_info.tasks),

		NewInfo =
		    Info#dm_info{tasks = NewTasks},

		process_task(NewInfo,RequestID,ConnectTask);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Connect system response (Receiver to Initiator)
	    {?DM,{ResponseID,
		  #system_connection_response{
		     id = ResponseID,
		     sent_by = Receiver,
		     result = Result,
		     conflicting_agents = Conflict,
		     agents_in_receiver_mas = ReceiverAgents,
		     
		     containers = Containers}}} ->
		
		case orddict:find(ResponseID,Info#dm_info.tasks) of
		    error ->
			Info; % message out of date. Ignored 
		    {ok, Task = #system_connection_initiator_task{
			   id = ResponseID,
			   container = Receiver}} ->
			%% Add Result, Containers and Conflict
			NewTask =
			    Task#system_connection_initiator_task{
			      result = Result,
			      conflicting_agents = Conflict,
			      agents_in_receiver_mas = ReceiverAgents,
			      containers_in_receiver_mas = Containers},
			
			process_task(Info, ResponseID, NewTask);
		    _ ->
			%% Receiver is not the receiver DM
			Info
		end;
	      
		    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% AgentList request

	    {?DM, {RequestID,
		   #agent_list_request{ id = RequestID,
				       answerTo = StakeHolder}}}->
		     Response =
			 #agent_list_response{ 
		       id = RequestID, 
		       sent_by = node(),
		       agent_list =
		       ordsets:union(Info#dm_info.agents,
				     Info#dm_info.reserved_names)      
		      },
		     
		     dm_send({?DM, StakeHolder},
			     {RequestID,Response}),
		     Info;    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% AgentList response

	    {?DM,{ResponseID,
		  #agent_list_response{
		    id = ResponseID,
		    agent_list = AgentList,
		    sent_by = Container}}} ->
		
		case orddict:find(ResponseID,Info#dm_info.tasks) of
		    error ->
			Info; % message out of date. Ignored 
		    {ok,Task = #system_connection_initiator_task{
			  pending_agent_list_containers = Pending,
			  gathered_agents =GatheredAgents} }->

			%% Remove one containers from the pending containers
		
			NewTask =
			    case ordsets:is_element(Container,Pending) of
				true ->
				    NewPending =
					ordsets:del_element(Container,Pending),

				    Task#system_connection_initiator_task{
				      pending_agent_list_containers = 
				      NewPending,
				      %% add the new agents received
				      gathered_agents = 
					  ordsets:merge(
					    fun(_,List1,List2) ->
						    lists:usort(List1++List2)
					    end,
					    GatheredAgents,
					    AgentList)};
				false ->
				    Task
			    end,
			process_task(Info,ResponseID,NewTask);
		    {ok,Task = #system_connection_receiver_task{
			  pending_agent_list_containers = Pending,
			  gathered_agents =GatheredAgents} }->
			
			%% Remove one container from the pending containers
			NewTask =
			    case ordsets:is_element(Container,Pending) of
				true ->
				    NewPending =
					ordsets:del_element(Container,Pending),
				    Task#system_connection_receiver_task{
				      pending_agent_list_containers = 
				      NewPending,
				      %% add the new agents received
				      gathered_agents = 
					  ordsets:merge(
					    fun(_,List1,List2) ->
						    lists:usort(List1++List2)
					    end,
					    GatheredAgents,
					    AgentList)};
				false ->
				    Task
			    end,
			process_task(Info,ResponseID,NewTask)
		end;
	     
		    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CheckAgents request (received by DMa)

	     {?DM, {RequestID,
		    #check_agents_request{
		      id = RequestID,
		      answerTo = Receiver}}} ->
		     
		     case orddict:find(RequestID,Info#dm_info.tasks) of
			 error ->
			     Info; % message out of date. Ignored 
			 {ok,Task= #system_connection_initiator_task{
			      id = RequestID,
			       container = Receiver}}->
			     
			     %% Change value of  check_agents_request_received

			     NewTask =
				 Task#system_connection_initiator_task{
				   check_agents_request_received = true},

			     NewTasks = orddict:store(RequestID,
						      NewTask,
						      Info#dm_info.tasks),

			     NewInfo = Info#dm_info{tasks = NewTasks},
			     process_task(NewInfo,RequestID,NewTask);
			 _ ->
			     %% Receiver is not the proper one
			     Info
		     end;
	      


		
	     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CheckAgents response (from DMa to DMb)
	    {?DM,{ResponseID,
		  #check_agents_response{
		    id = ResponseID,
		    agents = AgentList,
		    sent_by = Initiator}}}->
		     
		     case orddict:find(ResponseID,Info#dm_info.tasks) of
			 error ->
			     Info; % message out of date. Ignored 
			 {ok,Task = #system_connection_receiver_task{
			       id = ResponseID,
			       container = Initiator}}->
			     
			     %% Add AgentList

			     NewTask =
				 Task#system_connection_receiver_task{
				   check_agents_response_received = true,
				   agents_in_initiator_mas = AgentList},

			     NewTasks = orddict:store(ResponseID,
						      NewTask,
						      Info#dm_info.tasks),
			     
			     NewInfo = Info#dm_info{tasks = NewTasks},
			     process_task(NewInfo,ResponseID,NewTask)
		     end;
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Connect_to request (DMa to DM)
	      {?DM, {RequestID,
		     #connect_to_request{
		       id = RequestID,
		       answerTo = StakeHolder,
		       containers = ContainerList}}} ->

		     %% Skip a connection to itself
		     NewContainerList = ordsets:del_element(node(),
							 ContainerList),
         
		%% io:format("[DM] Node: ~p getting connected to ~p~n",
		%% 	  [node(), NewContainerList]),
		     connect_to(NewContainerList),
		     
		     Response =   #connect_to_response{
		       id = RequestID,
		       sent_by = node()
		      },
		     dm_send({?DM, StakeHolder},{RequestID,Response}),
		NewContainers = 
		    ordsets:union(NewContainerList,
				  Info#dm_info.containers),
		%% io:format("NewContainers: ~p~n",[NewContainers]),
		Info#dm_info{ % add containers
		       containers = NewContainers};
		    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Connect_to response (DM to DMa)
	    {?DM,{ResponseID,
		  #connect_to_response{
		    id = ResponseID,
		    sent_by = Container}}} ->
		
		
		case orddict:find(ResponseID,Info#dm_info.tasks) of
		    error ->
			
			Info; % message out of date. Ignored 
		    {ok,Task=#system_connection_initiator_task{
			  pending_connect_to_containers = Pending}} ->
			%% Erase pending
			NewPending =
			    ordsets:del_element(Container,
						Pending),
			
			NewTask = Task#system_connection_initiator_task{
				    pending_connect_to_containers= NewPending},
			
			%% io:format("Newtask: ~p~n",[NewTask]),
			NewTasks = orddict:store(ResponseID,
						 NewTask,
						 Info#dm_info.tasks),

			NewInfo = Info#dm_info{tasks = NewTasks},
			process_task(NewInfo,ResponseID,NewTask)
		end;
	     




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Get Info request
	    {?DM,  {RequestID,
		    #get_info_request{
		      id = RequestID,
		      answerTo = StakeHolder}}} ->

		Response = 
		    #get_info_response{
		  id = RequestID,
		  agents = Info#dm_info.agents,
		  containers = Info#dm_info.containers,
		  reserved_names = Info#dm_info.reserved_names,
		      sent_by = node()
		 },
		
		dm_send(StakeHolder,{RequestID,
				     Response}),
		Info;
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Print Info request
	    {?DM,  {_,#print_info_request{}}} ->

		pretty_print_info(Info),
		Info;
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DM down in a connected container

	    {'DOWN',_Ref,process, {?DM,Container}, Reason} ->
		case Reason of
		    noconnection ->
			%% Do nothing, as a nodedown shall be received as well
			Info;
		    _ ->
			io:format("[DM DEBUG:] Demonitoring node: ~p [~p] due"++
				      " to its distribution manager's death.~n",
				  [Container, Reason]),
			Info#dm_info{
			  containers =
			  ordsets:del_element(Container,
					      Info#dm_info.containers)	        
			 }
		end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Agent down  in the container

	    {'DOWN',_Ref,process, {AgentName,_Node},Reason} ->
		io:format("[DM DEBUG:] Demonitoring: ~p [~p]~n",[AgentName,Reason]),
		Info#dm_info{
		  agents =
		  ordsets:del_element(AgentName,
				      Info#dm_info.agents)
		  %% %%TODO: only erase this entry when a demonitor is invoked
		  %% monitored_agents =
		  %% orddict:erase(AgentName,Info#dm_info.monitored_agents)
		 };

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Container disconnected from this container

	    {'nodedown',Container} ->
		io:format("[DM DEBUG:] Demonitoring node: ~p~n",[Container]),	
		Info#dm_info{
		  containers =
		  ordsets:del_element(Container,
				      Info#dm_info.containers)		  
		 };


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%   Communication with the supervision manager
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% start_monitor_response

	    {?SM,{ResponseID, Response = #start_monitor_response{
				id = ResponseID}}}->
		
		case orddict:find(ResponseID,Info#dm_info.tasks) of
		    error ->
			
			Info; % message out of date. Ignored 
		
		    %% Message received as results of a create_agent req.
		    {ok, Task=#create_agent_task{
				 sm_notified = true,
				 sm_response = no_response}} ->
			
			NewTask = Task#create_agent_task{
				    sm_response = Response},
			
			%% io:format("Newtask: ~p~n",[NewTask]),
			NewTasks = orddict:store(ResponseID,
						 NewTask,
						 Info#dm_info.tasks),
			
			NewInfo = Info#dm_info{tasks = NewTasks},
			process_task(NewInfo,ResponseID,NewTask)
		end;
	    	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% erase_agent_request

	    {?SM,{RequestID, #erase_agent_request{
		    id = RequestID,
		    %% answerTo = StakeHolder,
		    dead_agent = AgentName}}}->

		%% io:format("[DM] Deleting agent: ~p~n",
		%% 	  [AgentName]),

		NewInfo = 
		    Info#dm_info{ 
		      %%delete the agent
		      agents =
		      ordsets:del_element(AgentName,
					  Info#dm_info.agents)
		     },
		
		Response =
		    #erase_agent_response{
		  sent_by = node(),
		  dead_agent = AgentName,
		  id = RequestID,
		  result = ?EJASONOK
		 },
		
		dm_send(?SM,{RequestID,Response}),
		NewInfo;
	     
		


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Discard other messages
    
	    Other ->
		io:format("[DM DEBUG:] Invalid Request/Response: ~p~n",[Other]),
		Info

	%after 0 ->
	%	timer:sleep(1000),
	%	Info
	end,
    ?DM:start(LoopInfo);


start(Info) -> 
    io:format("Error: Invalid info record: ~p~n",
	      [Info]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Handling changes in tasks/subtasks
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% Invoked when there have been changes on some stored task
%% i.g. an ?AGENTFOUND is returned for the find_agent subtask 
%% associated to some create_agent task
%% It sends the proper responses when suitable
%% Returns a new info record



%% SEND MESSAGE
process_task(Info= #dm_info{tasks = Tasks},
	     TaskID,
	     #send_message_task{
	       id = TaskID,
	       find_agent = #find_agent_task{
		 answerTo = StakeHolder,
		 agent_name = AgentName,
		 pending_containers = [],
		 found_in = no_found_in},
	       connection_attempted = true})->
    %% Recipient not found, connection already attempted

    %% 1) Notify the agent that executed .send

    Response = #send_message_response{
      id = TaskID,
      result = ?NOAGENT,
      sent_by = node(),
      agent_name = AgentName},
    
    dm_send(StakeHolder,{TaskID,Response}),
    
    %% Delete the send_message task
    NewTasks =
	orddict:erase(
	  TaskID,
	  Tasks),
    Info#dm_info{
      tasks = NewTasks};

process_task(Info= #dm_info{tasks = Tasks,
			   containers = Containers},
	     TaskID,
	     Task = #send_message_task{
	       id = TaskID,
	       connection_attempted = false,
	       find_agent = #find_agent_task{
		 pending_containers = [],
		 found_in = no_found_in},
	       suggested_container = SuggestedContainer})->
    %% Recipient not found, connection not attempted yet
    %% io:format("SendTask: ~p~n",[Task]),
    AllContainers = 
	ordsets:add_element(
	  node(),
	  Containers),
    
    case  ordsets:is_element(SuggestedContainer,AllContainers) of
	true->
	    %% do not try connection
	    NewTask =
		Task#send_message_task{connection_attempted = true},
	    process_task(Info,TaskID,NewTask);
	false ->

	    %% 1) Try connection (play as agent)
	    ConnectionID = connect(SuggestedContainer,TaskID),
	    
	    NewTask =
		Task#send_message_task{connection_attempted = true,
				       id_connection_task = ConnectionID},    
	    %% Store the modified send_message_task
	    NewTasks =
		orddict:store(
		  TaskID,
		  NewTask,
		  Tasks),
	    Info#dm_info{
	      tasks = NewTasks}
    end;

process_task(Info= #dm_info{tasks = Tasks},
	     TaskID,
	     #send_message_task{
	       id = TaskID,
	       find_agent = #find_agent_task{
		 answerTo = StakeHolder,
		 agent_name = AgentName,
		 pending_containers = [],
		 found_in = FoundIn}}) when FoundIn =/= no_found_in->
    %% recipient found

    %% %% 1) Send the message
    
    %% {AgentName,FoundIn} ! {communication,StakeHolder,node(),{Performative,
    %% 							        Message}},
    
   %% 1) Notify the agent that executed .send

    Response = #send_message_response{
      id = TaskID,
      result = ?AGENTFOUND,
      sent_by = node(),
      agent_name = AgentName,
      found_in_container = FoundIn},
    
    dm_send(StakeHolder,{TaskID,Response}),
    
    %% Delete the send_message task
    NewTasks =
	orddict:erase(
	  TaskID,
	  Tasks),
    Info#dm_info{
      tasks = NewTasks};
   

%%FIND AGENT
process_task(Info= #dm_info{tasks = Tasks},
	     TaskID,
	     #find_agent_task{
	       pending_containers = [],
	       agent_name = AgentName,
	       id = TaskID,
	       agent_pid = AgentPid,
	       answerTo = StakeHolder,
	       found_in = Result})->
    %% standalone find_agent_task is completed, send the proper answer

    Response =
	case Result of
	    no_found_in -> %% Not found
		#find_agent_response{ id = TaskID,
				      result = ?NOAGENT,
				      agent_name = AgentName
				     };				
	    _ ->
		#find_agent_response{ id = TaskID,
				      result = ?AGENTFOUND,
				      agent_name = AgentName,
				      agent_pid = AgentPid,
				      container = Result
				     }
	end,

    dm_send(StakeHolder,
	    {TaskID,Response}),
    
    
    %% Delete the find_agent task
    NewTasks =
	orddict:erase(
	  TaskID,
	  Tasks),
    Info#dm_info{
      tasks = NewTasks};

%%%%%%%%%%%% CREATE AGENT

%% SM responds
process_task(Info=#dm_info{tasks = InfoTasks,
			   containers = Containers},
	     TaskID,
	     CreateAgentTask = #create_agent_task{
	       id = TaskID,
	       sm_notified = true,
	       sm_response = #start_monitor_response{
		 result = Result,
		 monitored_agent = AgentName},
	       request = Request
	      }  ) ->

    case Result of
	
	?EJASONOK->
	    Response=
		#create_agent_response{ id = TaskID,
					sent_by = node(),
					result = ?CREATED,
					agent_name = AgentName,
					container = node(),
					code = Request#create_agent_request.code
				       },
	    
	    {AgentName,node()} ! {initialize,?DM},
	    dm_send(Request#create_agent_request.answerTo,
		    {TaskID,Response}),

	    %% Notify all ?SM in containers about the creation 
	    %% of the agent in order to
	    %% trigger the proper agent_up notifications (if applicable)
	    notify_agent_creation(AgentName,node(), [node()|Containers]),

	    NewInfo =
		Info#dm_info{
		  %% delete task
		  tasks = orddict:erase(TaskID,InfoTasks)},
	    NewInfo;
	_ ->
	    %%could not be monitored by SM, try again.
	    NewTask = CreateAgentTask#create_agent_task{
			sm_notified = true,
			sm_response = no_found_in},
	    NewInfo =
		%% add updated task
		Info#dm_info{tasks = ordsets:add_element(
				       TaskID,
				       NewTask,
				       InfoTasks)},
	    create_agent(NewInfo,Request)	     
    end;


%% last is_agent_response received
process_task(Info=#dm_info{tasks = Tasks},
	     TaskID,
	     CreateAgentTask = #create_agent_task{
	       id = TaskID,
	       %% sm has not been notified yet
	       sm_notified = false,
	       find_agent =  #find_agent_task{pending_containers = [],
					      found_in = Result},
	       request = Request
	      }  ) ->
    case Result of
	no_found_in ->
	    NewTask = CreateAgentTask#create_agent_task{
			sm_notified = true},
	    
	    NewInfo = Info#dm_info{tasks = orddict:store(TaskID,
							 NewTask,
							 Tasks)},
	    %% agent can be created,
	    create_agent(NewInfo, Request);
	_ ->
	    %% name clash
	    Response =
		#create_agent_response{ 
	      id = TaskID,
	      agent_name = Request#create_agent_request.agent_name,
	      container = Request#create_agent_request.container,
	      code = Request#create_agent_request.code,
	      result = ?NAMECLASH
	     },				

	    dm_send(Request#create_agent_request.answerTo,
		    {TaskID,Response}),
	    %% Delete the find_agent task
	    NewTasks =
		orddict:erase(
		  TaskID,
		  Tasks),
	    Info#dm_info{
	      tasks = NewTasks}
    end;




%% SYSTEM CONNECTION (initiator tasks)
process_task(Info=#dm_info{tasks = Tasks}, TaskID,
	     ConnectTask = #system_connection_initiator_task
	     {pending_agent_list_containers = [],
	      check_agents_request_received = true,
	      gathered_agents = GatheredAgents,
	      result = no_result,
	      container = ReceiverContainer}) ->
    %% All agents have been received from local MAS
    %% Check_agents_request has been received from receiver DM
    %% No result has been sent by the receiver DM

    %% Send check_agents_response to receiver DM

    %% io:format("[DM] Initiator gathered agents: ~p~n",[GatheredAgents]),

    %% My agents are added at the end to increase precision

    MyAgents = ordsets:union(Info#dm_info.agents,
			     Info#dm_info.reserved_names), 



    Response =
	#check_agents_response{ id = TaskID,
				sent_by = node(),
				agents = orddict:store(node(),
						       MyAgents,
						       GatheredAgents)},

    dm_send({?DM, ReceiverContainer},
	    {TaskID,Response}),

    %% No changes to the task ensue
    NewTasks =
	orddict:store(
	  TaskID,
	  ConnectTask,
	  Tasks),
    Info#dm_info{
	      tasks = NewTasks};

process_task(Info=#dm_info{tasks = Tasks,
			  containers = Containers}, TaskID,
	     ConnectTask = #system_connection_initiator_task
	     {
	       initiator_agent = Agent,
	       container = ReceiverContainer,
	       connect_to_containers_sent= false,
	       result = Result,
	       parent_task = ParentID,
	       conflicting_agents = Conflict,
	       gathered_agents = GatheredAgents,
	       agents_in_receiver_mas = ReceiverAgents,
	       containers_in_receiver_mas = ReceiverContainers}) 
  when Result =/= no_result  ->
    %% A result has been sent by the receiver DM
    %% connect_to_request has not been  sent yet

    case Result of 
	?EJASONOK ->

	    ReceiverAgentsList =
		orddict:to_list(ReceiverAgents),

	    GatheredAgentsList =
		orddict:to_list(GatheredAgents),
	    
	   
	    %% Function to notify a set of containers "ContainerList"
	    %% the creationg of each agent from ValueAgents within 
	    %% container KeyContainer

	    Notify = 
		fun (ContainerList, ContainerAgentList) ->
			lists:map(
			  fun ({KeyContainer, ValueAgents})->
				  notify_several_agent_creations(
				    ValueAgents, KeyContainer, ContainerList)
			    end,
			  ContainerAgentList)
		  end,
    

	    %% Notify my connected containers about the agents in 
	    %% the receiver MAS
	    Notify(Containers, ReceiverAgentsList),
	    

	    %% Notify the agents in the receiver MAS about the agents
	    %% in my connected container
	    %% io:format("[DM] Notifying ~p about agents ~p~n",
	    %% 	      [ReceiverContainer, GatheredAgentsList]),
	    Notify(ReceiverContainers, GatheredAgentsList),
	    
			    

	    %% Send connect_to requests
	    AllContainers =  ordsets:union(Containers,
					   ReceiverContainers),

	    Request = #connect_to_request{
	      id = TaskID,
	      answerTo = node(),
	      containers = ordsets:add_element(node(),AllContainers)},
	    
	    

	    lists:map(fun(ContainerName) ->
			      dm_send({?DM, ContainerName},
				      {TaskID,Request})
		      end,
		      AllContainers),
	    
	    NewTask =
		ConnectTask#system_connection_initiator_task{
		  connect_to_containers_sent= true,
		  pending_connect_to_containers = AllContainers},

	    NewTasks =
		orddict:store(TaskID,
			      NewTask,
			      Tasks),	  
	    connect_to(ReceiverContainers),
	    
	    NewInfo =
		 Info#dm_info{
		   containers = AllContainers,
		   tasks = NewTasks},

	    process_task(NewInfo,TaskID,NewTask);

	?NAMECLASH ->
	    %% send connection_response to agent
	    Response = 
		#connection_response{ id = TaskID,
				      sent_by = node(),
				      result = ?NAMECLASH,
				      parent_task = ParentID,
				      conflicting_agents = Conflict,
				      container = ReceiverContainer},

	    dm_send(Agent,
		    {TaskID,
		     Response}),

	    %% remove task
	    NewTasks =
		orddict:erase(TaskID,Tasks),

	    Info#dm_info{  tasks = NewTasks}
    end;

process_task(Info=#dm_info{tasks = Tasks}, TaskID,
	     #system_connection_initiator_task
	     {connect_to_containers_sent= true,
	      parent_task = ParentID,
	      initiator_agent = Agent,
	      pending_connect_to_containers = []}) ->
    %% Task completed

    %% Notify agent

    Response = #connection_response{
      id = TaskID,
      sent_by = node(),
      result = ?CONNECTED,
      parent_task = ParentID},
    
    dm_send(Agent,{TaskID,Response}),
    
      

    %% Remove the task
    
    NewTasks =
	orddict:erase(TaskID,Tasks),


    
    Info#dm_info{  tasks = NewTasks};	       

%% SYSTEM CONNECTION (receiver tasks)

process_task(Info=#dm_info{tasks = Tasks,
			  containers = Containers}, TaskID,
	     #system_connection_receiver_task
	     {pending_agent_list_containers = [],
	      result = no_result,
	      check_agents_response_received = true,
	      agents_in_initiator_mas = ODAgentsInitiator,
	      gathered_agents = ODGatheredAgents,
	      container = InitiatorContainer}) ->
    %% All agents have been received from local and initiator MAS
        
    %% Calculate and send the result:
    
    %% io:format("[DM DEBUG: connection process. ~nAgents in initiator: ~p~n"++
    %% 		  "Agents in receiver: ~p~n",[ODAgentsInitiator, 
    %% 					      ODGatheredAgents]),


    %% Add my own agents (done at the end, to increase precision)
    
    MyAgents = ordsets:union(Info#dm_info.agents,
			      Info#dm_info.reserved_names), 


    AgentsInitiator =
	ordsets:from_list(
	  orddict:fold( fun (_Key,Value, Acc) ->
			       Value++Acc end,
			[],
			ODAgentsInitiator)),
    
    GatheredAgents =
	ordsets:from_list(
	  orddict:fold( fun (_Key,Value, Acc) ->
			       Value++Acc end,
			[],
			orddict:store(node(), MyAgents, ODGatheredAgents))),


    case ordsets:intersection(AgentsInitiator,GatheredAgents) of
	[] ->
	    %% no clash, notify initiator DM 
	    
	    Response = 
		#system_connection_response{ 
		   id = TaskID,
		   sent_by = node(),
		   result = ?EJASONOK,
		   agents_in_receiver_mas = ODGatheredAgents,
		   containers = [node()|Containers]},
	    dm_send({?DM,InitiatorContainer},
		    {TaskID, Response});


	NameClashes ->
	    %% agent name clash
	    
	    Response = 
		#system_connection_response{ id = TaskID,
					     sent_by = node(),
					     result = ?NAMECLASH,
					     agents_in_receiver_mas = 
						 ODGatheredAgents,
					     conflicting_agents = NameClashes,
					     containers = Containers},
	    dm_send({?DM, InitiatorContainer},
		    {TaskID, Response})
    end,
	
    %% Just delete the task
    NewTasks =
	orddict:erase(TaskID,
		      Tasks),
    
    Info#dm_info{
      tasks = NewTasks};


process_task(Info=#dm_info{tasks = Tasks}, TaskID,
	     #system_disconnection_task
	     {pending_containers = [],
	      answerTo = StakeHolder}) ->
    %% All other containers have been disconnected from  me

    %% send response to agent
    Response =
	#disconnection_response{ id = TaskID,
				 sent_by = node()},

    dm_send(StakeHolder,{TaskID,
			 Response}),

    %% delete the task
	 	
    NewTasks =
	orddict:erase(TaskID,
		      Tasks),
    
    Info#dm_info{
      tasks = NewTasks};


%% TODO: check if this case can be replaced for an error check.
%% If no further actions are needed, maybe this function should not be
%% invoked at all. Check it!// July 2014
process_task(Info,TaskID,Task) ->
    %% No further actions needed, just add Task to Info
    Info#dm_info{
      tasks = orddict:store(TaskID,
			    Task,
			    Info#dm_info.tasks)}.


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Auxiliary Functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% %% Asks other containers to connect to the node
%% %% Returns a new task to be accomplished
%% request_connection(#dm_info{containers = Containers}, ParentID,
%% 		   ContainerList)->
%%     MyRequest =
%% 	#connect_to_request{ id = ParentID,
%%       			     **containers = ContainerList,
%% 			     answerTo = node()},
    
    
%%     lists:map(fun(Container) ->
%% 		     dm_send(Container,
%% 			     MyRequest)
%% 	      end,
%% 	      Containers),
%%     #task{
%% 	       type = ?CONNECTTO,
%% 	       request = MyRequest,
%% 	       parent_task = ParentID,
%% 	       pending_containers =
%% 	       Containers
%% 	      }.



%% Connect to a list of containers (nodes)
connect_to([]) ->
    ok;
connect_to([Container|List]) ->
    net_adm:ping(Container),
    erlang:monitor(process,{?DM,Container}),
    erlang:monitor_node(Container,true),
    connect_to(List).



%% Tests whether the agent name belongs to an agent already in the
%% container

%% Returns:
%%    ?NAMECLASH if the agent belongs to node(),
%%    ?NOAGENT if there are no other containers where it could belong
%%     #find_agent_task if a procedure to find the agent has been launched
check_name(Info,ParentID,AgentName)->
    
    case ordsets:is_element(AgentName,
			    lists:append([Info#dm_info.agents,
					  Info#dm_info.reserved_names])) of
	true ->
	    %% io:format("name_clash for ~p~n",[AgentName]),
	    ?NAMECLASH; 
	false -> 
	    case Info#dm_info.containers of
		[]->
		    %% io:format("no_agent for ~p~n",[AgentName]),
		    
		    ?NOAGENT;
		Containers->
		    %% io:format("start find_agent for ~p~n",[AgentName]),

		    %% Start a find_agent_task
		    IsAgentRequest =
			#is_agent_request{id = ParentID,
					  agent_name = AgentName,
					  answerTo = node()},
		    
		    lists:map(fun(Container) ->
				      dm_send({?DM, Container},
					      {ParentID,IsAgentRequest})
			      end, Containers), 
		    
		    
		    #find_agent_task{id = ParentID,
				     agent_name = AgentName,
				     pending_containers = Containers}
	    
	    end
    end.





%% Sends an agent_creation_notification to all ?DM in Containers
%% By now, these ?DM just bounce the message to the ?SM in their container
notify_several_agent_creations(AgentList, InContainer, Containers)->
    lists:map(fun (Agent) ->
		      notify_agent_creation(Agent, InContainer, Containers)
	      end,
	      AgentList).


%% Sends an agent_creation_notification to all ?DM in Containers
%% By now, these ?DM just bounce the message to the ?SM in their container
notify_agent_creation(AgentName, InContainer, Containers)->
    TS = erlang:now(),
    %% io:format("[DM] The containers are: ~p~n",[Containers]),
    Notification = #agent_creation_notification{id = TS,
						container = InContainer,
						agent_name = AgentName},
    lists:map(fun(Container) -> dm_send({?DM,Container}, {TS,Notification}) end,
	      Containers).


						 




%% % Tests if a container belongs to the system.
%% % Returns the new set of containers
%% disconnect_container(Info,Request) ->
%%     case ordsets:is_element(Request#request.info,Info#dm_info.containers) of
%% 	false ->
%% 	    Info#dm_info.containers; % set of containers maintained
%% 	true ->   	    
%% 	    lists:map(fun(OtherContainer) ->
%% 			      erlang:disconnect_node(OtherContainer)
%% 		      end,
%% 		      Info#dm_info.containers),
%% 	    [] % disconnected from all
%%     end.
		      



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
%% Procedure to connect two containers (Dma connects to DMb):
  
%%  1) DMa sends a system_connection_request to DMb
%%  2) DMa gathers all the agent names in its MAS1
%%  3) DMb sends a check_agents_request to DMa
%%  4) DMb gathers all the agent names in its MAS2
%%  5) DMa sends a check_agents_response to DMb
%%  6) DMb sends a system_connection_response to DMa
%%  7) DMa send a connect_to_request to ALL (MAS1 and MAS2) other containers
%%  8) DMa expects a connect_to_response from all other containers
%%  9) DMa sends a connection_response to agent

 %% Tests if a container belongs to the system.
 %% Returns either an agent_list_request, the atom ?CONNECTED or ?NOCONTAINER
connect_container(Info,RequestID,Container) ->
    %% io:format("Containers before connect: ~p~n",[Info#dm_info.containers]),
    case ordsets:is_element(Container,Info#dm_info.containers) of
	true ->
	    ?CONNECTED;
	false ->

	    case net_adm:ping(Container) of
		pong ->

		    MyRequest = % Request the connection to the other container
			#system_connection_request{ id = RequestID,
						    answerTo = node()
						   },

		     %% io:format("[DM ]Initiating connection to: ~p~n",
		     %% 	       [Container]),
		    dm_send({?DM, Container},{RequestID,MyRequest}),
		    %%io:format("Requesting agents to containers: ~p~n",
		    %%      [Info#dm_info.containers]),
		    gather_agents(Info,RequestID);
		pang ->
		    %% The container does not exist
		    ?NOCONTAINER
	    end
    end.


%% Polls the rest of DMs for the agent names in their container.
%% Returns the agent_list_request sent to these agents

gather_agents(#dm_info{containers = Containers},RequestID) ->
    MyRequest = 
	#agent_list_request{id = RequestID,
			    answerTo = node()},

    lists:map(fun(OtherContainer) ->
		      
		      dm_send({?DM,OtherContainer},
			      {RequestID,MyRequest})
	      end,
	      Containers), % poll the DMs in other containers

    MyRequest.




%% Creates a "unique" agent name that relies on the 
%% Erlang timestamp function "erlang:now" for its uniqueness
make_name()->
     "ejason_@"++variables:make_timestamp_string()++atom_to_list(node())++"@_ejason".





%% Creates a new agent (invoking spawn_agent) 
%% Returns a new dm_info record
create_agent(Info, #create_agent_request {
		     answerTo = StakeHolder,
		     id = CreateAgentID,
		     agent_name = AgentName,
		     container = Container,
		     code = Code})->
    			    
    case spawn_agent(AgentName,Code,CreateAgentID) of
	Pid when is_pid(Pid) ->
	    %% The agent is created, but the start message is not sent.
	    %% A start_monitor_response from SM must be received first
	    Info#dm_info{
	      %%add new agent and delete it from reserved names
	      reserved_names = ordsets:del_element(AgentName,
						   Info#dm_info.reserved_names),
	      agents = ordsets:add_element(AgentName,
					   Info#dm_info.agents)
	     };
	
	?NOCODE->
	    Response =
		#create_agent_response{ id = CreateAgentID,
					sent_by = node(),
					agent_name = AgentName,
					container = Container,
					code = Code,
					result = ?NOCODE
				       },
	    dm_send(StakeHolder,
		    {CreateAgentID,Response}),
 
	    Info#dm_info{
	      %% delete the agent name from reserved names
	      reserved_names = ordsets:del_element(AgentName,
						   Info#dm_info.reserved_names),
	      %% delete task
	      tasks =ordsets:del_element(CreateAgentID,Info#dm_info.tasks)
	     }
    end.
	    
	

%% Spawns an agent and informs ?SM (start_monitor_request)
%% Returns a PID or ?NOCODE 
%% TODO: currently, it assumes the asl code for an agent to be
%%       already parsed and compiled. Too strong an assumption. 
spawn_agent(AgentName,Code,TaskID) ->
    
    case whereis(AgentName) of
	undefined ->
	    ok;
	SomePid ->
	    exit(SomePid,kill) 
%%% kills it to avoid interferences from other Erlang processes not belonging to the platform
    end,
    
    %% Separate the filename and path.
    
    {Path,FileName} = utils:split_path(Code),
    
    OriginalPath = code:get_path(),
    
    %% Add the path to the beginning of the sourcecode path of Erlang, 
    %% if a path is given
    case Path of
	[] ->
	    ok;
	_ ->
	    case  code:set_path([Path]) of
		true ->
		    %% Path successfully changed
		    ok; 
		{error,_} ->
		    %% Path does not exist, then use "[]" as path
		    code:set_path([])
	    end
    end,
    
    %% io:format("Spawn Agent: ~p~nIn Path: ~p~nUsing Path: ~p~n",[FileName,Path,code:get_path()]),

    %% Note: avoid having source files with the same name in different folders
    %%       included in the path, in order to avoid conflicts.
    case code:ensure_loaded(list_to_atom(FileName)) of
    
        {module,_} ->
	    Pid = spawn(list_to_atom(FileName),start,[AgentName]),
	    %% io:format("[DM] Agent: ~p created with pid: ~p~n",
	    %% 	      [AgentName, Pid]),
	    %% Original Paths are restored
	    code:set_path(OriginalPath),
	    code:add_pathsz([Path]),
	    code:add_pathsa(["./"]),
	    
	    case whereis(AgentName) of 
		undefined ->
		    %% Register the new agent
		    try 
			register(AgentName,Pid),
			Request = #start_monitor_request{
				     id =TaskID,
				     answerTo = node(),
			  monitored_agent = AgentName},
			dm_send(?SM,{TaskID,Request}),
			Pid
		    catch
			_:_->
			    
			    case erlang:process_info(Pid) of
			     	undefined ->
				    
				    io:format("[DM DEBUG FATAL] ~p dies at init.\n",
			     	     	      [AgentName]),
				    spawn_agent(AgentName,Code,TaskID);
				_->
				    %%io:format("Kill1~n"),
				    %%Maybe some other process registered
				    %% causing a race condition		    
				    erlang:exit(Pid, kill),
				    spawn_agent(AgentName,Code,TaskID)
			 
			     end
			    
		    end;	       
	        SomeOtherPid  ->
		    %% Retry
		    %%io:format("Kill2~n"),
		    erlang:exit(Pid,kill),
		    erlang:exit(SomeOtherPid,kill),
		    spawn_agent(AgentName,Code,TaskID)
	    end;
	{error,_} ->
	    
	    %% Original Paths are restored
	    code:set_path(OriginalPath),
	    ?NOCODE
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CALL-BACK FUNCTIONS
%%
%% Note: they return and operation ID used to identify the suspended
%%       operations. 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_message(AgentName,SuggestedContainer)->
    SendID = erlang:now(),
    Request = 
	#send_message_request{ id = SendID,
			       answerTo = self(),
			       agent_name = AgentName,
			       suggested_container = SuggestedContainer},
			       %% performative=Performative,
			       %% message = Message},
       
    
    dm_send(?DM,{SendID,Request}),

    %% Returns the ID of the request
    SendID.
 


create_agent(AgentName,Container,Code)-> 
    Request = 
	#create_agent_request{ answerTo = self(),
			       agent_name = AgentName,
			       container = Container,
			       code = Code},
    
    CreateID = Request#create_agent_request.id,
    
    dm_send(?DM,{CreateID,Request}),
    %% io:format("Request: ~p~n",[Request]),
    %% Returns the ID of the request
    CreateID.
 

%% Return ?Stutter (notfound), ?Fail (no response) or 
%% {agentname, container,pid}  <- deprecated


% Returns the ID of the new FindAgentTask
find_agent(AgentName)->
    RequestID = erlang:now(),
    Request = 
	#find_agent_request{
      id = RequestID,
      agent_name = AgentName,
      answerTo = self()
     },
    dm_send(?DM,{RequestID,Request}),
    RequestID.


name_agent(Container,Code)->
    AgentName =
	make_name(),
    create_agent(AgentName,Container,Code).



%% Start a connection with Container
connect(Container) ->

    Request = 
	#connection_request{container  = Container,
			    answerTo = self()
			   },
    ConnectID = Request#connection_request.id,
    dm_send(?DM,{ConnectID,Request}),
    ConnectID.


%% Start a connection with Container (only available for DM, where ParentID
%% the ID of a send_message_task)
%%
%% TODO: try to merge with the previous function
connect(Container,ParentID) ->
    Request = 
	#connection_request{container  = Container,
			    answerTo = self(),
			    parent_task = ParentID
			   },
    ConnectID = Request#connection_request.id,
    dm_send(?DM,{ConnectID,Request}),
    ConnectID.




disconnect(Container) ->
    Request = 
	#disconnection_request{ container = Container,
				answerTo = self()
			       },
    DisconnectID = Request#disconnection_request.id,
    dm_send(?DM,{DisconnectID,Request}),
    DisconnectID.

   
get_info()->
    get_info(node()).

get_info(Container) ->
    RequestID = erlang:now(),
    Request = 
	#get_info_request{
      id = RequestID,
      answerTo = self()
     },
    dm_send({?DM,Container},{RequestID,Request}),
    RequestID.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% For DEBUGGING purposes
print_info()->
    RequestID = erlang:now(),
    Request = 
	#print_info_request{
      id = RequestID
     },
    dm_send(?DM,{RequestID,Request}),
    ok.

pretty_print_info(Info) ->
    io:format("\nDistribution Manager for container: ~p~n"++
	      "\tConnected to containers: ~p~n"++
	      "\tRegistered agents: ~p~n"++
	      "\tReserved agent names: ~p~n\n",
	      [node(),Info#dm_info.containers,
	       Info#dm_info.agents, Info#dm_info.reserved_names]).

%%%%%%%%%%%%


%% %% Invoked by the agents after the reception of a notification from a DM
%% process_response(#response{id = ID,
%% 			   info = Info,
%% 			   result = Result})->
%%     {ID,Info,Result}.

