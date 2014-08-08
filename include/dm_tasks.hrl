


%% Send procedure: 
%%   1) Find agent
%%   2) If agent not found and suggested_container not in MAS:
%%      2.1) Try connection
%%      2.2) Find agent
%%   3) If agent found, sent message
-record(send_message_task,
	{id = erlang:now(),
	 %% answerTo = answer_to_nobody, 
	 
	  agent_name = no_name, 
	 suggested_container = no_suggested_container,
	 %% performative = no_performative,
	 %% message = no_message,
	 find_agent = no_find_agent_subtask,
	 connection_attempted = false, %% steps 2.1-2.2 applicable?
	 id_connection_task = no_connection_task}).

	 %% pending_containers = [],
	 %% found_in = no_found_in}).



%% Create agent procedure: 
%%   1) Find agent
%%   2) If agent not found :
%%      2.1) Create Erlang process for agent and call erlang:register
%%      2.2) Send start_monitor_request to SM and wait for response
%%      2.3) Send {initialize,?DM} to the agent (then it starts)
%%   3) If agent found, fail (name clash)

-record(create_agent_task,
	{id = erlang:now(),
	 request = no_request, %% create_agent_request
	 sm_notified = false, %% notified  in ?DM:create_agent/2 by a start_monitor_request
         sm_response = no_response, %% #start_monitor_response
	 
	 answerTo = answer_to_nobody, %% duplicated, but more handleable
	 find_agent = no_find_agent_subtask %%subtask find_agent_task
	}).



-record(find_agent_task,
	{id = erlang:now(),
	 answerTo = answer_to_nobody, 
	 %%only used when is standalone task (not a subtask)  
	 agent_name = no_name, 
	 agent_pid = no_pid,
	 pending_containers = [],
	 found_in = no_found_in}).



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
%%  9) DMb sends a system_connection_response to DMa


-record(system_connection_initiator_task, %% DMa
	{id = erlang:now(),
	 initiator_agent = no_initiator_agent,
	 container = no_receiver_container,
	 agent_list_request = no_agent_list_request,
	 %% containers that have not sent their agent_name list yet
	 pending_agent_list_containers = [], 
	 gathered_agents = orddict:new(),

	 check_agents_request_received = false, %% eventually turned to true
	 %% Containers that will also be added to the MAS
	 containers_in_receiver_mas = [],

	 %% containers that have not sent a connect_to response yet
	 connect_to_containers_sent= false,
	 pending_connect_to_containers = [],
	 conflicting_agents = [],
	 
	 %% Agent names received in a system_connection_response
	 %% Necessary to send the proper agent_up notifications
	 agents_in_receiver_mas = orddict:new(),
	 parent_task = no_parent, %% only if initiated by a send

	 result = no_result}).


-record(system_connection_receiver_task, %% DMb
	{id = erlang:now(),
	 container = no_initiator_container,
	 agent_list_request = no_agent_list_request,
	 check_agents_response_received = false, %% eventually turned to true

	 %% containers that have not sent their agent_name list yet
	 pending_agent_list_containers = [], 
	
	 gathered_agents =orddict:new(),
	 result = no_result, %% the receiver calcultas this reponse
	 %% Agent names received in a check_agents response
	 agents_in_initiator_mas = []}).	 


-record(system_disconnection_task,
	{id = erlang:now(),
	 answerTo = answer_to_nobody, 
	 container = no_container,
	 pending_containers = []}).





%% -record(task, % TODO: try to avoid duplicated info e.g. name and request.info
%% 	{	  	  
%% 	  type = missing_type, % is the same as that of request, but is duplicated for convenience
%% 	  parent_task = is_parent,
	  
%% 	  task_info = no_task_info, % other info related to the task

%% 	  pending_containers = no_pending_containers, %containers that have not answered yet  743

%% 	  cumulative_agents =  no_cumulative_agents, % Used when request is of type  AGENTLIST 

%% 	  cumulative_containers =  no_cumulative_containers, %Cumulative containers for a CONNECT/CONNECTSYSTEM request 

%% 	  subtasks_ready = true,

%% 	  updated = erlang:now(),

%% 	  request = no_request% Request that started the task 
%% 	 }).
