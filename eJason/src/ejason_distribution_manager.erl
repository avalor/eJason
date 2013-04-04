-module(ejason_distribution_manager).

-export([start/0,
	 name_agent/2,
	 create_agent/3,
	 find_agent/1,
	 connect/1,
	 disconnect/1,
	 get_info/0, get_info/1,
	 process_response/1]).

-include("macros.hrl").
-include("dm_requests.hrl").
-include("sup_requests.hrl").


-record(info, % dm_info
	{agents = ordsets:new(),% Registered agents
	 containers = ordsets:new(),% Connected containers
	 cache = orddict:new(), % Name cache [{Name,Container}]
	 reserved_names = ordsets:new(), % Agent names not resolved yet
	 monitored_agents = orddict:new(),
	 tasks= orddict:new()
	 
	  }).    % tasks pending to proceed (name polls, connections...)


-record(task, % TODO: try to avoid duplicated info e.g. name and request.info
	{	  	  

	  type = missing_type, % is the same as that of request, but is duplicated for convenience
	  parent_task = is_parent,
	  
	  task_info = no_task_info, % other info related to the task

	  pending_containers = no_pending_containers, %containers that have not answered yet  

	  cumulative_agents =  no_cumulative_agents, % Used when request is of type  AGENTLIST 

	  cumulative_containers =  no_cumulative_containers, %Cumulative containers for a CONNECT/CONNECTSYSTEM request

	  subtasks_ready = true,

	  updated = erlang:now(),

	  request = no_request% Request that started the task 
	 }).





start()->
    register(?DM,self()),
    start(#info{}).

start(Info) when is_record(Info,info)->
    % TODO: consider using a gen_server
    % TODO create a record "request"
	receive % Returns a new Info record
	    Message ->
		Message
	end,
%    io:format("~n~n [DM] PROCESSING MESSAGE: ~n~p~n",
%	      [Message]),
    LoopInfo =
	case Message of

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Create Agent request (received by DMa from agent)

	    {?DM, Request = #request{type = ?CREATEAGENT,
				    info = {AgentName,_Code}}} ->
		%io:format("Request: ~p~n",[Request]),
		%io:format("Agents: ~p~n",[Info#info.agents]),
		    case check_name(Info,Request) of 
			?NAMECLASH  -> % name clash, respond now
			    Response =
				#response{ id = Request#request.id,
					   type = ?CREATEAGENT,
					   sentBy = node(),
					   info = Request#request.info,
					   result = ?NAMECLASH,
					   updated = erlang:now()	     
					  },				
			    dm_send(Request#request.answerTo,
				    Response),
			    Info;
			
			#task{pending_containers = []} -> % No more containers

			    create_agent(Info,Request);
			    
			SubTask = #task{request = IsAgentRequest}->
			    % no clash, store task while responses arrive
			    NewTask =
				#task{ request = Request,
				       subtasks_ready = false,
				       type = ?CREATEAGENT,
				       pending_containers=[]      			      
				      },
			    			    
			    Info#info{ % reserve the name, add two tasks
			      tasks =
			      orddict:store(
				Request#request.id,
				NewTask,
				orddict:store(IsAgentRequest#request.id,
					     SubTask,
					     Info#info.tasks)),
				
			      reserved_names = 
			      ordsets:add_element(
				AgentName,
				Info#info.reserved_names)}
		    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Find agent request (from agent to DMa)
	    {?DM, Request = #request{type = ?FINDAGENT}} ->
		case check_name(Info,Request) of 
		    ?NAMECLASH  -> % the agent is here, respond now
			Response =
			    #response{ id = Request#request.id,
				       type = ?FINDAGENT,
				       sentBy = node(),
				       info = {Request#request.info,node(),
					      whereis(Request#request.info)},
				       result = ?AGENTFOUND,
				       updated = erlang:now()	     
				      },				
			dm_send(Request#request.answerTo,
				Response),
			Info;
		    
		    #task{pending_containers = []} -> % No answer from other containers needed
			Response =
			    #response{ id = Request#request.id,
				       type = ?FINDAGENT,
				       sentBy = node(),
				       info = Request#request.info,
				       result = ?NOAGENT,
				       updated = erlang:now()	     
				      },				
			dm_send(Request#request.answerTo,
				Response),
			Info;
		    SubTask = #task{request = IsAgentRequest}->
			    % no clash, store task while responses arrive
			NewTask =
			    #task{ request = Request,
				   subtasks_ready = false,
				   type = ?FINDAGENT,
				   pending_containers=[]      			      
				  },
			    			    
			Info#info{ %add two tasks
			  tasks =
			  orddict:store(
			    Request#request.id,
			    NewTask,
			    orddict:store(IsAgentRequest#request.id,
					  SubTask,
					  Info#info.tasks))
			  }
		    end;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Is agent request
	    {?DM, Request = #request{type = ?ISAGENT,
				    info = AgentName}} ->
		case ordsets:is_element(AgentName,Info#info.agents) or
		    ordsets:is_element(AgentName,Info#info.reserved_names)
		    of
		    false ->
			Response =
				#response{ id = Request#request.id,
					   type = ?ISAGENT,
					   sentBy = node(),
					   info = Request#request.info,
					   result = ?NOAGENT,
					   updated = erlang:now()	     
					  },				
			dm_send(Request#request.answerTo,
				Response),
			Info;
		    true->
			Response =
				#response{ id = Request#request.id,
					   type = ?ISAGENT,
					   sentBy = node(),
					   info = {Request#request.info,node(),
						  whereis(AgentName)},
					   result = ?AGENTFOUND,
					   updated = erlang:now()	     
					  },				
			dm_send(Request#request.answerTo,
				Response),
			Info
		end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Is agent response when found
	    {?DM, #response{type = ?ISAGENT,
			    result = ?AGENTFOUND,
			    id = TS,
			    info = {AgentName,ContainerOfAgent,Pid},
			    sentBy = _Container}}   ->
				  
		case orddict:find(TS,Info#info.tasks) of
		    error ->
			Info; % message out of date. Ignored 
		    {ok,#task{parent_task = ParentID}} ->
			case orddict:find(ParentID, Info#info.tasks) of
			    error ->
				Info;
			    {ok,#task{type = Type, % CREATEAGENT or FINDAGENT
				     request = Request}} ->

				{ResponseInfo,
				 ResponseResult}=
				    case Type of 
					?CREATEAGENT ->
					    {{AgentName,ContainerOfAgent},
					     ?NAMECLASH};
					?FINDAGENT ->
					    {{AgentName,ContainerOfAgent,
					      Pid},
					     ?AGENTFOUND}    
				    end,

				MyResponse = 
				    #response{ id = ParentID,
					       type = Type,
					       sentBy = node(),
					       info = ResponseInfo,
					       result = ResponseResult,
					       updated = erlang:now()	     
					      },
				dm_send(Request#request.answerTo,MyResponse),
			    %  io:format("Info: ~p~nAgentName: ~p~n",[Info,AgentName]),
				Info#info{ %erase two tasks and reserved names
				  reserved_names =
				  ordsets:del_element(AgentName,
						      Info#info.reserved_names),
				  tasks =
				  orddict:erase(TS,
						orddict:erase(ParentID,
							     Info#info.tasks))
				 }
			end
		end;
	     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Is agent response when not found
	    {?DM, #response{type = ?ISAGENT,
			    result = ?NOAGENT,
			    id = TS,
			    sentBy = Container}}   ->		    
		case orddict:find(TS,Info#info.tasks) of
		    error ->
			Info; % message out of date. Ignored 
		    {ok,Task = #task{parent_task = ParentID, 
			     pending_containers = PendingContainers}} ->
			NewPending =
			    ordsets:del_element(Container,
						PendingContainers),
			case NewPending of
			    []-> % no more answers necessary, delete two tasks
				%io:format("2~n"),

				NewInfo =

				    Info#info{

				      tasks = orddict:erase(TS,
							    orddict:erase(
							      ParentID,
							      Info#info.tasks))				      
				     },
				
				case orddict:find(ParentID, Info#info.tasks) of
				    error ->
					NewInfo;
				    {ok,#task{type = ?CREATEAGENT,
					      request = Request}} ->
					create_agent(NewInfo,Request);
				    
				    {ok,#task{type = ?FINDAGENT,
					      request = Request}} ->
					MyResponse =
					    #response{ id = Request#request.id,
						       type = ?FINDAGENT,
						       sentBy = node(),
						       info = Request#request.info,
						       result = ?NOAGENT,
						       updated = erlang:now()	     
						      },				
					dm_send(Request#request.answerTo,
						MyResponse),
					NewInfo
				end;
			    			    
				     
			    _-> % more answers pending
				NewTask =
				    Task#task{
				      pending_containers = NewPending,
				      updated = erlang:now()
				     },
				Info#info{ % update task with new pending
				  tasks = orddict:store(TS,
							NewTask,
							Info#info.tasks)
				 }
			end
		end;
	    
				

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Connect request (from agent to DMa)
	    {?DM, Request = #request{type = ?CONNECT}} ->
		%io:format("Request: ~p~n",[Request]),
		case connect_container(Info,Request) of
		    ?CONNECTED -> % already connected
			Response =
			    #response{
			  id = Request#request.id,
			  type = ?CONNECT,
			  sentBy = node(),
			  result = ?CONNECTED		       
			 },
		   %Request = Task#task.request,
			dm_send(Request#request.answerTo,
				Response),
			%io:format("Connected already~n"),
			Info;



		    #task{pending_containers =
			  []}-> % no agents to gather
                                
			ConnectTask =
			    #task{
			  type = ?CONNECT,
			  pending_containers = [Request#request.info],
			  subtasks_ready =
			  true,
			  cumulative_agents = [],
			  request = Request},
			Info#info{
			  tasks =
			  orddict:store(
			    Request#request.id,
			    ConnectTask,
			    Info#info.tasks)
			 };
		    		
		    
		    ChildTask = #task{}-> % gathering agents
			ParentTask =
			    #task{
			  type = ?CONNECT,
			  pending_containers = [Request#request.info],
			  cumulative_agents = [],
			  subtasks_ready =
			  false,
			  request = Request}, 
			NewTasks =
			    orddict:store(
			      Request#request.id,
			      ParentTask,
			      Info#info.tasks),
			
			Info#info{
			  tasks =
			  orddict:store(
			    (ChildTask#task.request)#request.id, %ChildTask has a different id
			    ChildTask#task{parent_task = Request#request.id},
			    NewTasks)
			 }
		end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Disconnect request (from agent to DMa)
	    {?DM, Request = #request{type = ?DISCONNECT}} ->
		%io:format("Request: ~p~n",[Request]),
		
		NewContainers =
		    disconnect_container(Info,Request),
		
		Response =
		    #response{
		  id = Request#request.id,
		  type = ?DISCONNECT,
		  sentBy = node(),
		  result = ?DISCONNECTED		       
		 },
		
		dm_send(Request#request.answerTo,
			Response),		
		Info#info{ % update containers
		  containers = NewContainers
		 };
	    
		
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Connect system request (from DMa to DMb)
	    {?DM, Request = #request{type = ?CONNECTSYSTEM,
				    id = TS}} ->

		case Info#info.containers of
		    [] -> % no agents to gather
			
			CheckRequest = #request{ 
			  id = TS,
			  type = ?CHECKAGENTS,
			  answerTo = node(),
			  info = {ordsets:union(Info#info.agents,
						Info#info.reserved_names),
				  [node()]}		  
			 },

			dm_send(Request#request.answerTo,
			       CheckRequest),
			NewTask =
			    #task{
			  type = ?CONNECTSYSTEM,
			  request = Request,
			  cumulative_agents = [],
			  subtasks_ready = true,
			  pending_containers = [Request#request.answerTo]
			 },		    
			Info#info{ % add new checkagents task
			  tasks = 
			  orddict:store(TS, NewTask,
					Info#info.tasks)
			 };
		    
		    _ ->
			ChildTask =
			    gather_agents(Info),
		
			ParentTask =
			    #task
			  {type = ?CONNECTSYSTEM,
			  request = Request,
			  subtasks_ready = false,
			   cumulative_agents = [],
			  pending_containers = [Request#request.answerTo]
			 },
			
			NewTasks =
			    orddict:store(
			      Request#request.id,
			      ParentTask,
			      Info#info.tasks),
			
			Info#info{
			  tasks =
			  orddict:store(
			    (ChildTask#task.request)#request.id, %ChildTask has a different id
			    ChildTask#task{parent_task = Request#request.id},
			    NewTasks)
			 }
		end;    
			  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Connect system response
	    {?DM,#response{type = ?CONNECTSYSTEM,
			   id = TS}} ->
		
		case orddict:find(TS,Info#info.tasks) of
		    error ->
			Info; % message out of date. Ignored 
		    {ok, Task = #task{request = Request}} ->
			
			case Task#task.subtasks_ready of
			    false->
				NewTask =
				    Task#task{
				      pending_containers = [],
				      updated = erlang:now()
				     },
				Info#info{
				  tasks = orddict:store(
					    TS,
					    NewTask,
					    Info#info.tasks)					    
				 };
			    true -> % all connected
				Response =
				    #response{
				  id = TS,
				  type = ?CONNECT,
				  sentBy = node(),
				  result = ?CONNECTED			  
				 },
				dm_send(Request#request.answerTo,
					Response),

				Info#info{
				  tasks =
				  orddict:erase(TS,
						Info#info.tasks)
				 }
			end
		end;
		    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% AgentList request

	    {?DM, Request = #request{type = ?AGENTLIST}} ->
		Response =
		    #response{ id = Request#request.id,
			       type = ?AGENTLIST,
			       sentBy = node(),
			       info = 
			       ordsets:union(Info#info.agents,
					    Info#info.reserved_names),
			       result = ?EJASONOK,
			       updated = erlang:now()	     
			      },				
		dm_send(Request#request.answerTo,
			Response),
		Info;    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% AgentList response

	    {?DM,#response{type = ?AGENTLIST,
			   id = TS,
			   info = AgentList,
			   sentBy = Container}} ->
		
		case orddict:find(TS,Info#info.tasks) of
		    error ->
			Info; % message out of date. Ignored 
		    {ok,Task} when is_record(Task,task)->
			%io:format("TASK: ~p~n",
			%	  [Task]),
			Pending = 
			    ordsets:del_element(Container,
						Task#task.pending_containers),
		       
			NewAcumAgents =
			    ordsets:union(AgentList,
					  Task#task.cumulative_agents),
			%io:format("NewAcum = ~p~n",[NewAcumAgents]),
			%Request =
			 %   Task#task.request,
		       
			case Pending of 
			    [] -> % subtask ready, all agents gathered   
				MyTasks = Info#info.tasks,
				%% io:format("4: TS->~p~nTasks:~p~n",[TS,
				%% 	 MyTasks]),
				%% {test,'t@avalor-laptop.fi.upm.es'} !
				%%     TS,

				%% {test,'t@avalor-laptop.fi.upm.es'} !
				%%     MyTasks,

				NewTasks =
				    orddict:erase(TS,
						  MyTasks),
			
				% io:format("HECHO!!!!~n~n~n"),
				case orddict:find(Task#task.parent_task,Info#info.tasks) of
				    error ->	  
					Info#info{tasks = NewTasks};

				    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CHECKAGENTS req received
				    {ok, ParentTask = #task{
				      type = ?CONNECT,   
				     % request = #request{type = ?CONNECT},
						       pending_containers = [],
						       cumulative_agents = AgentList,
						       cumulative_containers = AcumContainers}} -> 
					NewInfo =
					    apply_merge_conditions(Info#info{tasks = NewTasks},
								   ParentTask, AgentList, 
								   NewAcumAgents,AcumContainers),
					NewInfo;
				    
						
                                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CHECKAGENTS req NOT received
				    
				    {ok,ParentTask = #task{type = ?CONNECT}}  -> 
					Info#info{ % update task saying that subtask is ready
					  tasks =
					  orddict:store(
					    Task#task.parent_task,
					    ParentTask#task{cumulative_agents = NewAcumAgents,
							    subtasks_ready = true},
					    NewTasks)
					 };
			    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% If parent is CONNECTSYSTEM, I send CHECKAGENTS req
				    {ok,ParentTask = #task{type = ?CONNECTSYSTEM}} ->
					AnnouncedAgents = 
					    ordsets:union([Info#info.agents, Info#info.reserved_names,
							  NewAcumAgents]),
					%io:format("Info: ~p~nAnnounced: ~p~n",[Info,AnnouncedAgents]),
					MyRequest = 
					    #request{
					  id = Task#task.parent_task,
					  type = ?CHECKAGENTS,
					  answerTo = node(),
					  info = {
					    AnnouncedAgents, % agents
					    ordsets:add_element(node(),
								Info#info.containers
							       ) % containers
					   }
					 },
					
					dm_send((ParentTask#task.request)#request.answerTo,
						MyRequest),
					NewConnSysTask =
					    ParentTask#task{
					      pending_containers = [(ParentTask#task.request)#request.answerTo],
					      updated = erlang:now(),
					      cumulative_agents = AnnouncedAgents
					     },
					Info#info{ 
					  tasks =
					  orddict:store(Task#task.parent_task,
							NewConnSysTask,
							NewTasks)}
				end; %case of find(parent_task)

			   _ -> % If Pending =/= []
				Info#info{% add new cumulative agents, delete one pending container
				  tasks = 
				      orddict:store(TS,
						    Task#task{
						      pending_containers = Pending,
						      cumulative_agents =  NewAcumAgents,
						      updated = erlang:now()
						     },
						    Info#info.tasks)
				 }
			end % case Pending
		end; % case find(TS)
		    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CheckAgents request (received by DMa)

	    {?DM, #request{type = ?CHECKAGENTS,
			   answerTo = DMb,
			   info = {AgentNames,Containers},
			   id = TS}} ->
%	io:format("Checkagents Request: ~p~nContainers: ~p~n",
%	  [R,Info#info.containers]),
		case orddict:find(TS,Info#info.tasks) of	    
		    {ok,Task= #task{pending_containers = [DMb]}}->
			%Pending = 
			%    Task#task.containers,
			
			case Task#task.subtasks_ready of
			    false -> % still gathering agents, just store those received
			%	io:format("subtasks pending~n"),
 				NewTask =
				    Task#task{
				      
				      pending_containers = [],
				      cumulative_agents = AgentNames,
				      cumulative_containers = Containers,
				      updated = erlang:now()
				     },
				Info#info{
				  tasks = orddict:store(TS,NewTask,
							Info#info.tasks)
				 };
			    true -> % everything gathered, check for name clashes and respond
				NewInfo =
				    apply_merge_conditions(Info,
							   Task, AgentNames, 
							   Task#task.cumulative_agents,Containers),
				NewInfo
			end;
		    _ ->
			io:format("CHR ignored!~n"),
			Info % message ignored
		end;
				
	     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CheckAgents response (from DMa to DMb)
	    {?DM,#response{type = ?CHECKAGENTS,
			   id = TS,
			   info = Containers,
			   sentBy = DMa,
			   result = Result}} ->
		
		case orddict:find(TS,Info#info.tasks) of
		    error ->
			Info; % message out of date. Ignored 
		    {ok,Task} when is_record(Task,task)->
						%io:format("NewContainerset: ~p~n",
						%	  [ordsets:union(ContainerList,
						%			 Info#info.containers)]),
				
			case Result of
			    ?NAMECLASH -> % just delete task
				%io:format("5~n"),

				Info#info{
				  tasks = orddict:erase(
					    TS,
					    Info#info.tasks)
				 }; 
			    
			    ?EJASONOK ->
				connect_to(Containers),
				
				case Info#info.containers of 
				    []-> % No containers to request connection, just answer ok
					
					Response =
					    #response{ id = TS,
						       type = ?CONNECTSYSTEM,
						       sentBy = node(),
						       result = ?EJASONOK			  
						      },
					
					dm_send(DMa,Response),
									%io:format("6~n"),

					Info#info{ % add containers and remove task
					  containers =
					  ordsets:union(Info#info.containers,
							Containers),
					  tasks =
					  orddict:erase(
					    TS,
					    Info#info.tasks)
					 };  
				    _ -> % request connection and add subtask
					 
					
					ChildReqTask =
					    request_connection(Info,TS,
							       Containers),
					ListWithReq =
					    orddict:store(
					      (ChildReqTask#task.request)#request.id,
					      ChildReqTask,
					      Info#info.tasks),
					NewConnectTask = 
					    Task#task{
					      cumulative_containers = 
					      Containers,
					      pending_containers = [],
					      subtasks_ready = false,
					      updated = erlang:now()
					     },
					Info#info{ % add containers and two new tasks
					  containers =
					  ordsets:union(Containers,
							Info#info.containers),
					  tasks =  orddict:store(
						     TS,
						     NewConnectTask,
						     ListWithReq)
					 }
				end % case Info.containers
			end % case result
		end; % case orddict
		    
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Connectto request

	    {?DM, Request = #request{type = ?CONNECTTO,
				     info = ContainerList,
				     answerTo = Container}} ->		       
		connect_to(ContainerList),
	      	%io:format("Connecting to : ~p~n",[ContainerList]),
		Response =   #response{
		  id = Request#request.id,
		  type = ?CONNECTTO,
		  sentBy = node(),
		  result = ?EJASONOK
		 },
		dm_send(Container,Response),
		Info#info{ % add containers
		  containers =
		  ordsets:union(ContainerList,
				Info#info.containers)};
		    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Connectto response
	    {?DM,#response{type = ?CONNECTTO,
			   id = TS,
			   sentBy = Container}} ->
		
		
		case orddict:find(TS,Info#info.tasks) of
		    error ->
			Info; % message out of date. Ignored 
		    {ok,Task} when is_record(Task,task)->
			Pending =
			    ordsets:del_element(Container,
						Task#task.pending_containers),
		       
			Request = Task#task.request,
			
			case Pending of
			    []->% all connected
				%io:format("ALL CONNECTED~n"),
				case orddict:find(Task#task.parent_task,Info#info.tasks) of
				    {ok,#task{type = ?CONNECTSYSTEM,
					  request = PRequest}} -> % must send response to DMa (PRequest.id)
					Response =
					    #response{
					  id = PRequest#request.id,
					  type = ?CONNECTSYSTEM,
					  sentBy = node(),
					  result = ?EJASONOK			  
					 },

					dm_send(PRequest#request.answerTo,
						Response),
			       

					Info#info{ % erase two tasks
					  tasks =
					  orddict:erase(
					    PRequest#request.id,
					    orddict:erase(TS,
							  Info#info.tasks))
					 };

				    {ok,#task{type = ?CONNECT,
					  request = PRequest,
					  pending_containers =[]}} -> %send response to agent (Request.id)
					Response =
					    #response{
					  id = PRequest#request.id,
					  type = ?CONNECT,
					  sentBy = node(),
					  result = ?EJASONOK			  
					 },

					dm_send(Request#request.answerTo,
						Response),
				

					Info#info{ % erase two tasks
					  tasks =
					  orddict:erase(
					    PRequest#request.id,
					    orddict:erase(TS,
							  Info#info.tasks))
					 };
				    {ok,ParentTask = #task{type = ?CONNECT}} -> %must wait for connectsystem resp.
					NewTask =
					    ParentTask#task{
					      subtasks_ready = true,
					      updated = erlang:now()
					     },
				

					Info#info{ % update one task, delete other
					  tasks =
					  orddict:store(Task#task.parent_task,
							NewTask,
							orddict:erase(TS,
								      Info#info.tasks))
					 };
				    error ->
					Info#info{
					  tasks = 
					  orddict:erase(TS,% delete the subtasks: no parent task
							Info#info.tasks)
					 }
				       
				end;	
				    
				
			    _ -> % more responses pending
				NewTask =
				    Task#task{
				     pending_containers = Pending,
				      updated = erlang:now()
				     },
				Info#info{ % just update task
				  tasks =
				  orddict:store(TS,
						NewTask,
						Info#info.tasks)
				 }
			end

		end;
	    			

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Get Info request
	    {?DM, Request = #request{type = ?GETINFO}} ->
		Response = 
		    #response{
		  type = ?GETINFO,
		  id = Request#request.id,
		  info = {Info#info.agents,
			  Info#info.containers,
			  Info#info.reserved_names},
		  sentBy = node(),
		  result = ?EJASONOK
		 },
		
		dm_send(Request#request.answerTo,Response),
		Info;
	    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Agent down  in the container

	    {'DOWN',_Ref,process, {AgentName,_Node},Reason} ->
		io:format("[DM DEBUG:] Demonitoring: ~p [~p]~n",[AgentName,Reason]),
		Info#info{
		  agents =
		  ordsets:del_element(AgentName,
				      Info#info.agents)		  
		 };

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Container disconnected from this container

	    {'nodedown',Container} ->
		io:format("Demonitoring node: ~p~n",[Container]),	
		Info#info{
		  containers =
		  ordsets:del_element(Container,
				      Info#info.containers)		  
		 };


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%   Communication with the supervision manager
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

	    {?SM, #sm_mon_request{
		type = ?MONITORED,
		id = TS,
		%monitoring_agent = Monitor,
		monitored_agent = Monitored				  
	       }} ->
		
		Response =
		    #sm_mon_response{
		  type = ?MONITORED,
		  sentBy = node(),
		  id = TS,
		  result = ?EJASONOK
		 },
		dm_send(whereis(?SM),Response),
		
		case orddict:find(Monitored,
				  Info#info.monitored_agents) of
		    {ok,Ref} ->
			erlang:demonitor(Ref), %NOTE: maybe add [flush]
			Info#info{ %remove the monitor
			  monitored_agents =
			  orddict:erase(Monitored,
					Info#info.monitored_agents)
			 };
		    
		    error->
			Info
		end;
	    
		








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Discard other messages
    
	    Other ->
		io:format("Invalid Request/Response: ~p~n",[Other]),
		Info

	%after 0 ->
	%	timer:sleep(1000),
	%	Info
	end,
    start(LoopInfo);



start(Info) -> 
    io:format("Error: Invalid info record: ~p~n",
	      [Info]).





%% Asks other containers to connect to the node
%% Returns a new task to be accomplished
request_connection(#info{containers = Containers}, ParentID,
		   ContainerList)->
    MyRequest =
	 #request{type=?CONNECTTO,
		  info = ContainerList,
		  answerTo = node()},
    
    
    lists:map(fun(Container) ->
		     dm_send(Container,
			     MyRequest)
	      end,
	      Containers),
    #task{
	       type = ?CONNECTTO,
	       request = MyRequest,
	       parent_task = ParentID,
	       pending_containers =
	       Containers
	      }.



%% Connect to a list of containers (nodes)
connect_to([]) ->
    ok;
connect_to([Container|List]) ->
    net_adm:ping(Container),
    monitor_node(Container,true),
    connect_to(List).



% Tests whether the agent name belongs to an agent already in the
% container
check_name(Info,Request)->
    Name =
	case Request#request.info of
	    {AgentName,_Code} ->
		AgentName;
	    AgentName when is_atom(AgentName) ->
		AgentName
	end,
	
    case ordsets:is_element(Name,Info#info.agents) or
	 ordsets:is_element(Name,Info#info.reserved_names)
	of 
	true -> 
	    %io:format("1~n"),
	    ?NAMECLASH; 
	false -> 
	    %io:format("2~n"),

	    poll_name(Info#info.containers, Request)
    end.


% Polls the rest of DMs for the name. It contains a timestamp to
% resolve possible conflicts


poll_name(Containers,
	  #request{id =TS, 
		   info = Name}) ->

    UseName =
	case Name of
	    {AgentName,_Code} ->
		AgentName;
	    _ when is_atom(Name) ->
		Name
	end,

    MyRequest =
	#request{type=?ISAGENT,
		 info = UseName,
		 answerTo = node()},

    lists:map(fun(Container) ->
		      dm_send(Container,
			      MyRequest)
	      end,
	      Containers), % poll the DMs in other containers
    #task{% name = Name,
	  type = ?ISAGENT,
	  parent_task = TS,
	  pending_containers = Containers,
	  request = MyRequest}. % create new pending task
	
    
% Tests if a container belongs to the system.
% Returns the new set of containers
disconnect_container(Info,Request) ->
    case ordsets:is_element(Request#request.info,Info#info.containers) of
	false ->
	    Info#info.containers; % set of containers maintained
	true ->   	    
	    lists:map(fun(OtherContainer) ->
			      erlang:disconnect_node(OtherContainer)
		      end,
		      Info#info.containers),
	    [] % disconnected from all
    end.
		      


% Tests if a container belongs to the system.
% Returns either a task or the atom ?CONNECTED
connect_container(Info,Request) ->
    case ordsets:is_element(Request#request.info,Info#info.containers) of
	true ->
	    ?CONNECTED;
	false ->
	    MyRequest = % Request the connection to the other container
		#request{
	      id = Request#request.id,
	      type = ?CONNECTSYSTEM,
	      answerTo = node()
	     },
	    
	    dm_send(Request#request.info,
		    MyRequest),
	    %io:format("Requesting agents to containers: ~p~n",
		%      [Info#info.containers]),
	    gather_agents(Info)
    end.


% Polls the rest of DMs for the agent names in their container.
% Returns a task to be added to the set of pending tasks.
% This task is the child of another derived from a CONNECT/CONNECTSYSTEM request

gather_agents(#info{containers = Containers}) ->
    MyRequest = 
	#request{type=?AGENTLIST,
		 answerTo = node()},

    lists:map(fun(OtherContainer) ->
		      
		      dm_send(OtherContainer,
			      MyRequest)
	      end,
	      Containers), % poll the DMs in other containers
    #task{ type = ?AGENTLIST,
	   pending_containers = Containers,
	   cumulative_agents = ordsets:new(),
	  %task_info = , % {ack,agentList,AgentsThere,ContainersThere}
	  request = MyRequest}. % create new pending task





%% Creates a "unique" agent name that relies on the 
%% Erlang timestamp function "erlang:now" for its uniqueness
make_name()->
    List = tuple_to_list( erlang:now()), 
    [A,B,C] =
	lists:map(fun make_string_of_six/1, List),
    
    "ejason_@"++A++B++C++atom_to_list(node())++"@_ejason".

make_string_of_six(Int) when is_integer(Int)->
    String = integer_to_list(Int),
    case string:len(String) of
	Len when Len < 6 ->
	    make_string_of_six(String);
	Len when Len == 6 ->
	    String
    end;
make_string_of_six(Str) when is_list(Str) ->
    String = "0"++Str,
    case string:len(String) of
	Len when Len < 6 ->
	    make_string_of_six(String);
	Len when Len == 6 ->
	    String
    end.


% Sends a message to some recipient appending the tag ?DM
% Several clauses depending on the recipient
dm_send(Pid, Message) when is_pid(Pid)-> %% Between a DM and an agent
    %io:format("~nSending Message: ~p~n To: ~p~n~n",[Message,Pid]),
    Pid ! {?DM,Message};
dm_send(Container, Message) when is_atom(Container)-> % Between DMs
    %io:format("~nSending Message: ~p~nTo: ~p~n~n",[Message,{?DM,Container}]),
    {?DM,Container} ! {?DM,Message};
dm_send(Other,_Message) ->
    io:format("[DM]Invalid Recipient: ~p~n",[Other]).



% Creates a new agent and sends the proper response to the agent
create_agent(Info, Request = #request {info = {AgentName,Code}})->
    Response =
	#response{ id = Request#request.id,
		   type = ?CREATEAGENT,
		   sentBy = node(),
		   info = Request#request.info,
		   result = ?CREATED,
		   updated = erlang:now()	     
		  },	
			    
    NewRef =
	spawn_agent(AgentName,Code),
    
    
    dm_send(Request#request.answerTo,
	    Response),
 
    Info#info{ %add new agent and delete it from reserved names
               % add new reference to the monitored agent
      reserved_names = ordsets:del_element(AgentName,Info#info.reserved_names),
      agents = ordsets:add_element(AgentName,
				   Info#info.agents),
      monitored_agents = orddict:store(
			   AgentName,
			   NewRef,
			  Info#info.monitored_agents)
     }.


% just spawn an agent 
spawn_agent(AgentName,Code) ->
    case whereis(AgentName) of
	undefined ->
	    ok;
	SomePid ->
	    exit(SomePid,kill) % kills it to avoid interference from other Erlang processes not in the platform
    end,
    Pid = spawn(Code,start,[AgentName]),
    register(AgentName,Pid),
    erlang:monitor(process,{AgentName, node()}).



% Checks whether the conditions for a merge hold. Sends response to DMb. 
% Starts CONNECTTO subtask if applicable.
% @Info the current DM state
% @Task is the CONNECT task
% @ReceivedAgentList list of the agents received from DMb
% @GatheredAgentList lisf ot the agents gathered by DMa
% @Containers containers in the platform of DMb
apply_merge_conditions(Info,Task, ReceivedAgentList, 
		       GatheredAgentList,Containers)->
    

    Request = Task#task.request,
    
    Result = case ordsets:intersection(
		    GatheredAgentList,
		    ReceivedAgentList) of 
		 [] -> % no name clash
		     ?EJASONOK;
		 
		 _Agents ->
		     ?NAMECLASH
	     end,
    Response =
	#response{ id = Request#request.id,
		   type =  
		   ?CHECKAGENTS,
		   sentBy = node(),
		   info = 
		   ordsets:add_element(
		     node(),
		       Info#info.containers
		    ),
		   result = Result},        
    
    dm_send(Request#request.info, % send response to DMb
	    Response),
    case Result of
	?NAMECLASH ->
		%		io:format("9~n"),

	    Info#info{
	      tasks = orddict:erase(
			Request#request.id,
			Info#info.tasks)
	     };
	?EJASONOK ->
	    connect_to(Containers),
	    case Info#info.containers of
		[]-> % no containers to request connection
		    NewConnectTask = 
			Task#task{
			  cumulative_containers = 
			  Containers,
			  pending_containers = 
			  [Request#request.info],
			  subtasks_ready = true,
			  updated = erlang:now()
			 },
		    Info#info{ % add containers and update task
		      containers =
		      ordsets:union(Info#info.containers,
				    Containers),
		      tasks =
		      orddict:store(
			Task#task.parent_task,
			NewConnectTask,
			Info#info.tasks)
		     };
		_ -> % more containers in the platform. Send a CONNECTTO request and create subtask
		    ChildReqTask =
			request_connection(Info,Request#request.id,
					   Containers),
		    ListWithReq =
			orddict:store(
			  (ChildReqTask#task.request)#request.id,
			  ChildReqTask,
			  Info#info.tasks),
		    NewConnectTask = 
			Task#task{
			  cumulative_containers = 
			  Containers,
			  pending_containers = 
			  [Request#request.info],
			  subtasks_ready = false,
			  updated = erlang:now()
			 },
		    Info#info{ % add containers and two new tasks
		      containers =
		      ordsets:union(Containers,
				    Info#info.containers),
		      tasks =
		      orddict:store(
			Request#request.id,
			NewConnectTask,
			ListWithReq)
		     }
	    end % case Info.containers
    end. % case Result of




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CALL-BACK FUNCTIONS


create_agent(AgentName,Container,Code)-> 
% TODO allow these operations asynchronously
% TODO info: the requests have a 10 secs timeout by now...
   Result =
	if
	    Container == node() -> 
		ok;
	    true ->
		ID = get_info(), % check if Container is connected
		receive 
		    {?DM, #response{id = ID,
				    info = {_Agent,Containers,_Reserved}
				   }} ->
			case lists:member(Container,Containers) of
			    true ->
				ok;
			    _ ->
				not_connected
			end
		after 10000 ->
			not_connected
		end
	end,
    case Result of
	ok ->
	    Request = 
		#request{
	      type = ?CREATEAGENT,
	      info = {AgentName,Code},
	      answerTo = self()
	     },
	    dm_send(Container,Request),
	    CreateID = Request#request.id,
	    CreateID;

	not_connected ->
	    {?FAIL}
    end.

%% Return ?Stutter (notfound), ?Fail (no response) or 
%% {agentname, container,pid}  <- deprecated


% Returns the ID of the new FindAgentTask
find_agent(AgentName)->
    Request = 
	#request{
      type = ?FINDAGENT,
      info = AgentName,
      answerTo = self()
     },
    dm_send(node(),Request),
    FindAgentID = Request#request.id,
    FindAgentID.


name_agent(Container,Code)->
    AgentName =
	make_name(),
    create_agent(AgentName,Container,Code).


connect(Container) ->

    Request = 
	#request{
      type = ?CONNECT,
      info = Container,
      answerTo = self()
     },
    dm_send(node(),Request),
    ConnectID = Request#request.id,
    ConnectID.




disconnect(Container) ->
    Request = 
	#request{
      type = ?DISCONNECT,
      info = Container,
      answerTo = self()
     },
    dm_send(node(),Request),
    Request#request.id.
 

get_info()->
    get_info(node()).

get_info(Container) ->
    Request = 
	#request{
      type = ?GETINFO,
      %info = ,
      answerTo = self()
     },
    dm_send(Container,Request),
    Request#request.id.



% Invoked by the agents after the reception of a notification from a DM
process_response(#response{id = ID,
			   info = Info,
			   result = Result})->
    {ID,Info,Result}.

