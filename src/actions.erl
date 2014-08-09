-module(actions).

-export([ internal_action/4,
	  external_action/3,
	  test_goal/3,
	  no_wait_test_goal/3]).

-include("include/parser.hrl").
-include("include/variables.hrl").
-include("include/macros.hrl").
-include("include/records.hrl").
-include("include/ejason.hrl").
-include("include/suspendedActionsRecords.hrl").



%% Test goal in a plan formula that can be retried(??Query)
test_goal(AgentInfo,Bindings,Query)->
    case belief_base:query_bb(AgentInfo,Bindings,Query) of
	false -> 
	    utils:add_test_goal(Bindings,Query);
	It when is_function(It) -> 
	    case iterator:first(It) of
		false -> %% Could not be resolved
		    utils:add_test_goal(Bindings,Query);
		NewBindings when is_list(NewBindings)->
		    {replace_bindings,
		     iterator:create_iterator([NewBindings])}
	    end
    end.
   
%% Test goal in a plan formula that fails if it cannot be matched (?Query) 
no_wait_test_goal(AgentInfo,Bindings,Query)->
    case belief_base:query_bb(AgentInfo,Bindings,Query) of
	false -> 
	    ValuatedQuery = variables:valuate(Bindings, Query),
	    #event{type =?FAILEDTESTGOAL,
		   body = ValuatedQuery};
	    %% io:format("Not Resolved: ~p~n",[Query]),
	    %% {?FAIL};
	It when is_function(It) -> 
	    case iterator:first(It) of
		false -> %% Could not be resolved
		    ValuatedQuery = variables:valuate(Bindings, Query),
		    #event{type =?FAILEDTESTGOAL,
			   body = ValuatedQuery};
		    %% io:format("Noti Resolved: ~p~n",[Query]),
		    %% {?FAIL};
		NewBindings when is_list(NewBindings)->
		    {replace_bindings,
		     iterator:create_iterator([NewBindings])}
	    end
    end.





%% %% TODO: study applicability of annotations in external actions
%% external_action(EnvironmentName,ExAc = {#var{id = ActName},Args,Annot})->
%%     %% Fun = fun (X) ->
%%     %% 		   %% case binds_for_vars(X) of
%%     %% 		  %%     {Value,{},[]}->
%%     %% 		  %% 	  Value;
%%     %% 		  %%     Other->
%%     %% 		  %% 	  Other
%%     %% 		  %% end
%%     %% 	  end,	
    
    
%%     Params = Args,%lists:map(Fun, tuple_to_list(Args)),
%%     %Annotations = list_to_tuple(lists:map(Fun, Annot)),
%%     TimeStamp = erlang:now(), % Will serve as identifier for the 
%%                                % response message
    
%%  %   Annotations = lists:map(Fun,Annots),
   
%% %    io:format("[utils] EX Ac Params: ~p~n",[Params]),
%%    % io:format("Action: ~p:~p~n",[EnvironmentName,ActName]),
%%     ExecutingProcess = 
%% 	spawn(environment_handler,
%% 	      execute,
%% 	      [EnvironmentName,
%% 	       ActName,Params,Annot,self(),TimeStamp]),%% Continue
%%     {suspend,ExecutingProcess,TimeStamp}.



external_action(_AgentInfo,Bindings,Var) ->
    case variables:valuate(Bindings,Var) of
	#var{args = ?ISATOM, 
	     functor = true} ->
	    {replace_bindings,iterator:create_iterator([Bindings])};
	Other ->
	    io:format(
	      "[WARNING]: The current implementation of eJason does not\n"++
	      "           allow the execution of external actions.\n"++
	      "           The execution of the action in ~p fails.~n~n",
	      [Other]),
	    
	    {?FAIL}
    end.

    

%% Executes an internal action
%% Return {replace_bindings,ItBindings}, {?FAIL} or 
%% {ACTIONSUSPEND,ID,SuspendedAction}
internal_action(AgentInfo, OriginalBindings,
		Package= '.', InAc) -> 
    %% = #var{functor = ActName,
    %%	     args = Args,
    %%	     annots = Annot})->
    %%InAc = {#var{id = ActName},Args,Annot}) ->
    %%   io:format("Package: ~p~n",[Package]),
       %% io:format("InternalAction: ~p ~n",[InAc]),
      %% io:format("Original ~p Bindings: ~p ~n",[length(OriginalBindings),
      %% 					       OriginalBindings]),

    try	  
	%% io:format("Original Internal ~p~n",
	%% 	  [InAc]),

	%% io:format("Original Bindings ~p~n",
	%% 	  [OriginalBindings]),

    {Bindings, CorrectedInternalAction} = 
	variables:correct_structs(OriginalBindings,InAc),
 
   %% io:format("NewInternalAction: ~p ~n",[CorrectedInternalAction]),

   %%  io:format("NewBindings: ~p~n",[Bindings]),

    
    InternalAction = variables:valuate(Bindings,
				      CorrectedInternalAction),
 	
	%% io:format("Internal ~p~n",
	%% 	  [InternalAction]),
						 
	%% #var{functor = ActName,r
     	%%  args = Params,
     	%%  annots = Annot} =  InternalAction,
    case
	execute(AgentInfo,Bindings,Package,
		InternalAction) of
	{?STUTTERACTION} -> %% Body formulas corrected
	    {replace_bindings,iterator:create_iterator([Bindings])};
	{?FAIL} ->
	    {?FAIL};
	{replace_bindings,ItNewBindings} when is_function(ItNewBindings) ->
	    {replace_bindings,ItNewBindings};
	{?ACTIONSUSPEND,ID,SuspendedAction} ->
	    %% The internal action suspends the intention. 
	    %% It will return from here as soon as possible.
	    %% TODO: consider this case when invoking not INTACT or similar
	    {?ACTIONSUSPEND,ID,SuspendedAction};
	Other ->
	    io:format("Actions.erl: Other: ~p~n",[Other]),
            timer:sleep(3000),
	    {?FAIL}
    end 
    catch
	%% A:B ->
	%%     io:format("A: ~p~nB: ~p~n",[A,B]);
	exit:improper_list ->
	    {?FAIL}

    end;
internal_action(_Agent,_OriginalBindings,
		Package, _InAc) -> %% External Action
    
    io:format("Package: ~p~n",[Package]),
     PackagePath =
     	lists:join(
     	  [atom_to_list(Class) || {atom,_,Class} <- Package],
     	  "."),
    io:format("[WARNING]: The current implementation of eJason does not\n"++
	      "           allow the execution of external actions.\n"++
	      "           The execution of the action in ~s fails.~n~n",
	     [PackagePath]),
    
    {?FAIL}.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% INTERNAL ACTIONS (in alphabetical order)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Procedure to add a new internal action .new_name(Args1..ArgN):

%% 1) Add at least one pattern  execute(AgentInfo,Bindings,'.'
%%                                      #var{functor = #var{args = ?ISATOM,
%% 			              functor = new_name},
%%                               	     args = {Args1...ArgsN}})

%% 2) If it cannot be executed straightaway (interaction with DM or SM needed),
%%    a #suspended_new_name record must be added to the file 
%%    "suspendedActionsRecords.hrl"
  
%%   2.1) Add a new pattern for 
%%       reasoningCycle:process_dm_response(Agent,
%% 					 Response,
%% 					 {#suspended_new_name{..},
%% 					  SuspendedIntention})

%% 3) Handle the result in Response
  



%% Connect to a container
execute(_AgentInfo,_Bindings,'.',
	#var{functor = #var{args = ?ISATOM,
			    functor = connect},
	     args = {Container},
	     annots =_})->
    
    ContainerName =
	case Container of
	    %% Container is given like container[label...]
	    #var{functor =#var{args = ?ISATOM,
			       functor = Name},
		 args = Args} when is_tuple(Args) ->
		Name;
	    #var{args = ?ISATOM,
		 functor = Name}->
		Name
	end,
    		

    %%io:format("[actions.erl]~p connection requested ~n",[self()]),
    ConnectID =
	?DM:connect(ContainerName),
    ID = {?DM,ConnectID},
    SuspendedAction = 
	#suspended_connect{
      container = ContainerName
     },
    {?ACTIONSUSPEND,ID,SuspendedAction};

%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% CONTAINER for agent AgentName
execute(_AgentInfo,Bindings,'.', #var{functor =
			     #var{args = ?ISATOM,
				  functor = container_of_agent},
			     args ={AName,Container}})->
  
    try
	AgentName = 
	case AName of
	    #var{args = ?ISATOM, functor = AtomName} ->
		AtomName;
	    
	    #var{functor = #var{args = ?ISATOM,functor =AtomName},
		 args = {}}->
		AtomName
	end,
	
	
	FindID = ?DM:find_agent(AgentName),

	ID = {?DM,FindID},
	SuspendedAction = 
	#suspended_find_container{
	  agent_name = AgentName,
	  container_var = Container,
	  use_bindings = Bindings
	 },
	{?ACTIONSUSPEND,ID,SuspendedAction}
	
    catch
	%% e.g. .container([1,2,3],_,_).
	error:{case_clause,_Error} ->
	    %% io:format("container Error~p~n",[_Error]),
	    {?FAIL}
    end;

%%%%%%%%%%%%%%%%%%%%%

%% CONTAINERS in the system

execute(_AgentInfo,Bindings,'.', #var{functor =
				      #var{args = ?ISATOM,
					   functor = containers},
				      args ={Containers}})->
  
    	

    GetID = ?DM:get_containers(),

    ID = {?DM,GetID},
    SuspendedAction = 
	#suspended_get_containers{
      containers_var = Containers,
      use_bindings = Bindings
     },
    {?ACTIONSUSPEND,ID,SuspendedAction};

	
  
%%%%%%%%%%%%%%%%%%%%%


%% CREATE a new agent
execute(AgentInfo,Bindings,'.', 
	CreateVar = #var{functor = #var{args = ?ISATOM,
					functor = create_agent},
			 args={AName, ParamCode}})->
   %% No customisations
    execute(AgentInfo, Bindings,'.', 
	    CreateVar#var{
	      args ={AName, ParamCode,no_custom_properties}}
	   );	

execute(_AgentInfo,Bindings,'.',
	#var{functor = #var{args = ?ISATOM,
			    functor = create_agent},
	     args =  {AName,ParamCode,_Custom},
	     annots = _Label})->
    try
	
	{AgentName,Node} = 
	case AName of
	    #var{args = ?ISATOM, functor = AtomName} ->
		{AtomName,node()};
	    
	    #var{functor = #var{args = ?ISATOM, functor = AtomName},
		 args = {},
		 annots = Annots}->
		%% Check if the annotations specify another container.

		ContainerName =
		    variables:find_container_name(Bindings,Annots),
		
		 %% io:format("[actions] Specified ContainerName: ~p~n",
		 %% 	  [ContainerName]),
		
		{AtomName,ContainerName}
	end,
    %%io:format("utils2~p~n",[ParamCode]),
    %%io:format("InputNode: ~p~n",[InputNode]),
    %%io:format("New2!!~n~n"),
	%% io:format("[actions.erl] Creating agent with name: ~p in node ~p~n",
	%% 	  [AgentName,Node]),

	Code = 
	case ParamCode of
	    #var{args = ?ISATOM, functor =StringCode} when is_list(StringCode)->
		StringCode
	end,
	
	%% TODO: check whether code can be found!!
	
	CreateID =?DM:create_agent(AgentName,Node,Code),
	ID = {?DM,CreateID},

	%% Creates a suspended action. Waits for the DM to answer
	SuspendedAction = 
	#suspended_create_agent{
	  agent_name = AgentName,
	  container = Node,
	  code = Code
	 },
	{?ACTIONSUSPEND,ID,SuspendedAction}
	
    catch
	%% e.g. .create_agent([1,2,3],_,_).
	error:{case_clause,_Error} ->
	    io:format("Create_agent Error~p~n",[_Error]),
	    {?FAIL}
    end;

 
    
%%%%%%%%%%%%%%%%%%%

%% DISCONNECT from a container
execute(_AgentInfo,_Bindings,'.',
	#var{functor = #var{args = ?ISATOM,
			    functor = disconnect},
	     args={Container}})->
    
    ContainerName =
	case Container of
	    %% Container is given like container[label...]
	    #var{functor =#var{args = ?ISATOM,
			       functor = Name},
		 args = Args} when is_tuple(Args) ->
		Name;
	    #var{args = ?ISATOM,
		 functor = Name}->
		Name
	end,
    %io:format("[~p]connection requested ~n",[self()]),
    DisconnectID =
	?DM:disconnect(ContainerName),
    ID = {?DM,DisconnectID},
    SuspendedAction = 
	#suspended_disconnect{
      container = ContainerName
     },
    {?ACTIONSUSPEND,ID,SuspendedAction};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% MAKE THE PLAN FAIL
execute(_AgentInfo,_Bindings,'.',
	#var{functor = fail, args = ?ISATOM})->
    {?FAIL};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FIND another agent 
execute(_AgentInfo,_Bindings,'.',#var{functor = #var{args = ?ISATOM,
						     functor = find_agent},
				      args = {AName}}) ->
   
%% AName can be an agent's name or a structure AgentName[Container]
%% io:format("Finding: ~p~n",[AName]),

    try
	AgentName =
	case AName of
	    #var{args = ?ISATOM}->
		AName#var.functor;
	    #var{args = {},
		 functor = #var{args = ?ISATOM,
				functor = FuncAtom}}->
		FuncAtom
	end,
	
	FINDID = ?DM:find_agent(AgentName),
	ID = {?DM,FINDID},
	SuspendedAction = 
	#suspended_find_agent{
	  agent_name = AgentName},
	{?ACTIONSUSPEND,ID,SuspendedAction}	
    catch
	%% e.g. .find_agent([1,2,3]) or .find_agent(UnboundVar)
	error:{case_clause,_} ->
	    {?FAIL}
    end;	


%%%%%%%%%%%%%%%%


%% KILL another agent (the process running it)
%% It will be carried out by the SM
execute(#agent_info{agent_name = KillingAgent},
	_Bindings,'.',#var{functor = #var{args = ?ISATOM,
					  functor = kill_agent},
			   args = {AName}}) ->
    
%% AName can be an agent's name or a structure AgentName[Container]
%    io:format("Killing: ~p~n",[AName]),

    try
	AgentName =
	case AName of
	    #var{args = ?ISATOM}->
		AName#var.functor;
	    #var{args = {},
		 functor = #var{args = ?ISATOM,
			    functor = FuncAtom}}->
		FuncAtom
	end,
	
	

	KillID = ?SM:kill_agent(AgentName, KillingAgent),
	ID = {?SM,KillID},
	SuspendedAction = 
	    #suspended_kill_agent{ 
	       dying_agent_name = AgentName,
	       killing_agent_name = KillingAgent},
	{?ACTIONSUSPEND,ID,SuspendedAction}

	%% KillID = ?DM:find_agent(AgentName),
	%% ID = {?DM,KillID},
	%% SuspendedAction = 
	%% #suspended_kill_agent{
	%%   agent_name = AgentName},
	%% {?ACTIONSUSPEND,ID,SuspendedAction}	
    catch
	%% e.g. .kill_agent([1,2,3]).
	error:{case_clause,_} ->
	    {?FAIL}
    end;	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% MONITOR another agent (the process running it) [.monitor(Agent)]
execute(AgentInfo,
	Bindings,'.',
	MonitorVar = #var{functor = #var{args = ?ISATOM,
					 functor = monitor_agent},
			  args = {AgentVar}}) ->
    
    execute(AgentInfo, Bindings, '.',
	    MonitorVar#var{args = {AgentVar, ?PERSISTANY}});

%%%% MONITOR another agent (the process running it with options) 
%%%%  [.monitor(Agent,Configuration)]
execute(#agent_info{agent_name = MonitoringAgent},
	Bindings,'.',
	#var{functor = #var{args = ?ISATOM,
			    functor = monitor_agent},
	     args = {AgentVar,ConfigurationVar}}) ->
    
    try 
	MonitoredAgent = case AgentVar of
		      #var{args = ?ISATOM}->
			  AgentVar#var.functor;
		      #var{args = {},
			   functor = #var{args = ?ISATOM,
					  functor = FuncAtom}}->
			  FuncAtom
		  end, 

	Options = 
	    variables:find_monitor_options(Bindings,ConfigurationVar),
	
       	
	%%io:format("~p monitors ~p ~n",[MyName, RegName]),
	
	MonitorID = ?SM:monitor(MonitoringAgent, MonitoredAgent, Options),
	ID = {?SM,MonitorID},
	SuspendedAction = #suspended_monitor{ monitoring_agent = MonitoringAgent,
					      monitored_agent = MonitoredAgent,
					      options = Options},
	{?ACTIONSUSPEND,ID,SuspendedAction}

    catch
	%% e.g. .monitor([1,2,3],abc).
	error:{case_clause,_} ->
	    {?FAIL}	
    end;



%%%%%%%%%%%%%%%%

%% PRINT some string in the standard output
execute(#agent_info{agent_name=MyName},
	_Bindings,'.',#var{functor = #var{args = ?ISATOM,
					  functor = print},
			   args = String})-> 
    %% io:format("IMPRIMIENDO ~p~n",[String]),
    NewString =     
	print_list(tuple_to_list(String)),
    
    %%     io:format("[~p:~p]: ~p~n",[MyName,node(),NewString]),
    io:format("[~p]: ~s~n",[MyName,NewString]),
    %%    io:format("[~p,~p]: ~s",[Name,erlang:now(),NewString]),
    {?STUTTERACTION};



%%%%%%%%%%%%%%%%


%% SEND a message to some other agent
execute(#agent_info{agent_name = MyName},Bindings,
	'.',#var{functor = #var{args = ?ISATOM,
				functor = send},
		 args = {Receiver,
			 Performative,Message}})->
    
    %% Receiver can be an agent's name or a structure  AgentName[Container]
    %%io:format("Receiver: ~p~n",[Receiver]),
    
    try    
	{AgentName,SuggestedContainer}=
	
	case Receiver of
	    #var{args = ?ISATOM, functor = AtomName} ->
		{AtomName,node()};
	    
	    #var{functor = #var{args = ?ISATOM,functor =AtomName},
		 args = {}, annots = Annots} ->
		%% send(Name[container(SomeContainer)])
		{AtomName,variables:find_container_name(Bindings,Annots)}
	end,
	
	NewPerformative = % performative
	case Performative of
	    #var{args = ?ISATOM,
		 functor = AtomPerf}->
		AtomPerf;
	    #var{functor = #var{args = ?ISATOM,functor =AtomPerf},
		 args = {}} ->
		AtomPerf
	end,
	
	%%io:format("~p receiver is: {~p,~p}~n",[erlang:now(),ReceiverID,node()]),
	%%io:format("{Intention,Message} is: {~p,~p}~n",[NewIntention,Message]    
	
	SendMessageID = ?DM:send_message(AgentName,SuggestedContainer),
	ID = {?DM,SendMessageID},
	SuspendedAction = 
	#suspended_send{
	  my_name = MyName,
	  receiver_name = AgentName,
	  container = SuggestedContainer,
	  performative = NewPerformative,
	  message = Message
	 },
	{?ACTIONSUSPEND,ID,SuspendedAction}

	
    catch
	%% e.g. .send([1,2,3],performative(3),message).
	error:{case_clause,_} ->
	    {?FAIL}
    end;

%%%%%%%%%%%%%%%%


%%% TO_NUMBER Turns a string into a number
execute(_AgentInfo,Bindings,'.', #var{functor = #var{args = ?ISATOM,
						     functor = to_number},
				      args = {
					#var{args=?ISATOM,
					     functor=Value},
					ResultVar}}) ->
  try
   
      
    Number = case Value of
		 _ when is_list(Value)->
		     try
			 list_to_integer(Value)
		     catch
			 _:_->
			     list_to_float(Value)
		     end;
		 _ when is_float(Value)->
		     Value;
		 _ when is_integer(Value)->
		     Value	    
	     end,
    
      NumberVar = #var{args = ?ISATOM,
		       functor = Number,
		       id = Number},
      
      UseBindings =   orddict:store(Number,
				    NumberVar,
				    Bindings),
      
      case variables:match_vars(UseBindings,NumberVar,ResultVar) of
	  false ->
	      {?FAIL};
	  
	  ItNewBindings ->
	    {replace_bindings,ItNewBindings}
      end
      
  catch
      _:_->
	  {?FAIL}
  end;  


%%%%%%%%%%%%%%%%

%% TO_STRING Turns a term into string
execute(_AgentInfo,Bindings,'.', #var{functor = #var{args = ?ISATOM,
						     functor = to_string},
				      args = {Value,ResultVar}}) ->

   	
    StringVar = #var{args = ?ISATOM,
		     functor = print_elem(Value),
		     id = "STRINGVARFROMINTACTION"++
		     variables:make_timestap_string()},
    UseBindings =
	orddict:store(
	  StringVar#var.id,
	  StringVar,
	  Bindings),
    


    case variables:match_vars(UseBindings,StringVar,ResultVar) of
	false ->
 	    {?FAIL};
	
	ItNewBindings ->
	    {replace_bindings,ItNewBindings}
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%

%% WAIT: agent sleeps during WaitTime milliseconds

%%TODO: do not make the agent sleep unless the action is invoked within
%%      a critical section
execute(_AgentInfo,_Bindings,'.',
	#var{functor =#var{args = ?ISATOM,
			   functor = wait},
	     args ={WaitTime}}) ->
    
    try
	
	Time =
	case WaitTime of
	    #var{args= ?ISATOM} ->
		WaitTime#var.functor
	end,
	%% io:format("Sleeping: ~p~n",[round(Time)]),
	timer:sleep(round(Time)),
	{?STUTTERACTION}

    catch
	%% e.g. .wait([1,2,3]).
	error:{case_clause,_} ->
	    {?FAIL}
    end;


  


%%%%%%%%%%%%%% Private queries (name, container...)

%% GET Name  by returning the registered name of the erlang process running it
execute(#agent_info{agent_name = MyName}, Bindings,
	'.', #var{functor =#var{args = ?ISATOM,
				functor = my_name},
		  args ={QueryVar}}) -> 
    
     ResultVar =
	#var{functor = MyName,
	     id = MyName,
	     args = ?ISATOM,
	     annots = []},
    UseBindings =
	orddict:store(
	  MyName,
	  ResultVar,
	  Bindings),
    
    case variables:match_vars(UseBindings,ResultVar,QueryVar) of
	false ->
	    {?FAIL};	
	ItNewBindings ->
	    {replace_bindings,ItNewBindings}
    end;

%% GET Container name by returning the name of the node the agent runs on
execute(_AgentInfo,Bindings,'.', #var{functor =
			     #var{args = ?ISATOM,
				  functor = my_container},
			     args ={QueryVar}})->
    MyContainer = node(),
    ResultVar =
	#var{functor = MyContainer,
	     id = MyContainer,
	     args = ?ISATOM,
	     annots = []},


   UseBindings =
	orddict:store(
	  MyContainer,
	  ResultVar,
	  Bindings),
    

    case variables:match_vars(UseBindings,ResultVar,QueryVar) of
	false ->
	    {?FAIL};
	
	ItNewBindings ->
	    {replace_bindings,ItNewBindings}
    end;




%% GET The environment file that interfaces to the set of external actions
execute(#agent_info{environment = MyEnvironment},Bindings,
	'.',#var{functor =
		 #var{args = ?ISATOM,
		      functor = my_environment},
		 args ={QueryVar}}) ->

  
    ResultVar =
	#var{functor = MyEnvironment,
	     id = MyEnvironment,
	     args = ?ISATOM,
	     annots = []},

   UseBindings =
	orddict:store(
	  MyEnvironment,
	  ResultVar,
	  Bindings),
    

    case variables:match_vars(UseBindings,ResultVar,QueryVar) of
	false ->
		{?FAIL};
	    
	    ItNewBindings ->
		{replace_bindings,ItNewBindings}
    end;
    

execute(_AgentInfo,_Bindings,'.',Action) ->
    %%TODO: include error messages for the different actions
    %%     e.g. "some action needs N params, only N-1 were given"...
    io:format("[actions.erl] Undefined internal action: ~p~n",[Action]),
    {?FAIL};
execute(_AgentInfo,_Bindings,Package,Action) ->
    %% io:format("Undefined Action: ~p in Package ~p~n",[Action,Package]),
    case 
	environment_handler:execute_internal(Package,Action) of
	{?FAIL} ->
	    {?FAIL};
	{ok,NewBindings} ->
	    {update_bindings,iteraro:create_iterator([NewBindings])}
    end.






%%% AUXILIARY FUNCTIONS
print_list([])->
    "";
print_list(List)->
   %% io:format("List: ~p~n",[List]),
    Fun = fun (X) ->
		  case X of 
		      %% X is a string
		      #var{id = '[]', args = ?ISLIST} ->
			  "[]";
		      #var{functor = Func, args = ?ISATOM} when is_list(Func) ->
			  Func;
		      _ ->
			  print_elem(X) 
		  end
	  end,
    lists:flatten(lists:map(Fun,List)).
    
    
%%Turns variables into strings to be printed 

print_elem(?EMPTYLISTVAR)->
    "[]";
print_elem(Var = #var{functor = Func, args = ?ISATOM}) ->   
    if
	is_atom(Func) ->
	    atom_to_list(Func);
	is_float(Func)->
	    float_to_list(Func);
	is_integer(Func) ->
	    integer_to_list(Func);
	is_boolean(Func)->
	    if 
		Func ->
		    "true";
		true ->
		    "false"
	    end;
	is_list(Func) ->
	    "\""++Func++"\"";
	true ->
	    io:format("Var in print: ~p~n",
		      [Var]),
	    "DEBUG:WRONGELEMENTINPRINT"
    end;

print_elem(#var{functor = StructVar,
		args = ?STRONGNEG}) ->
    "~"++print_elem(StructVar);		    
print_elem(#var{functor = {Header,Tail},
	       args = ?ISLIST}) ->
    %% io:format("PRINTINGLIST: ~p~n",
    %% 	      [List]),
    "["++ 
	string:join(lists:map(fun (X) -> 
				      print_elem(X) end,
			      Header), ",")++
	case print_list(Tail) of
	    "[]" ->
	    	"";
	     []-> %% The tail is the ejason empty list
	    	"";		    
	    StringTail ->
		
		%% Tail is a list, strip it of its brackets

		%%io:format("StringTail: ~p~n",[StringTail]),
		case string:str(StringTail,"UNBOUNDVAR") of
		    1 -> %% The tail is an unbound var and does not carry []
			","++StringTail;
		    _ ->
			","++string:substr(StringTail,2,length(StringTail)-2)
		end
		%% Adding a comma
		%% [44|string:substr(StringTail,
		%% 	      2,
		%% 	      length(StringTail)-2)]
	   % Other ->
  	    %	Other
	end ++		      
	"]";
print_elem(#var{functor = ?NOFUNCTOR, args = ?UNBOUND, id = ID}) ->
    StringID =
	atom_to_list(ID),
    Res =  case string:str(StringID,"Ejason_") of
	       1 ->
		   string:substr(StringID,
				 8)++"<no value>";
	       _ ->
		   case string:str(StringID,"EJASONREPLACEDVAR") of
		       1 ->
			   lists:flatten( ["UNBOUNDVAR",
					   string:substr(StringID,
							 18),"<no value>"]);   
		       _ ->StringID++"<no value>"

		   end
	   end,
    %io:format("UNBOUNDVAR PRINTED AS: ~s~n",[Res]),
    Res;
print_elem(#var{functor = ?NOFUNCTOR, args = ?UNBOUNDLIST, id = ID}) ->
    StringID =
	atom_to_list(ID),
    Res =  case string:str(StringID,"Ejason_") of
	       1 ->
		   string:substr(StringID,
				 8)++"[LIST]<no value>";
	       _ ->
		   case string:str(StringID,"EJASONREPLACEDVAR") of
		       1 ->
			   lists:flatten( ["UNBOUNDVAR",
					   string:substr(StringID,
							 18),
					   "[LIST]<no value>"]);   
		       _ ->StringID++"[LIST]<no value>"

		   end
	   end,
    %io:format("UNBOUNDVAR PRINTED AS: ~s~n",[Res]),
    Res;
print_elem(#var{functor = Func, args = Args,annots = Annots}) ->
    StringFunc = 
	print_elem(Func),    
    
    StringArgs =
	case Args of
	    {} ->
		"";
	    _ ->
		"("++string:join(
		       lists:map(fun print_elem/1,
				 tuple_to_list(Args)),
		       ",")++")"
	end,
    
    StringAnnots = 
	case Annots of
	    [] ->
		"";
	    _ ->
		"["++string:join(
		       lists:map(fun print_elem/1,
				 Annots),
		       ",")++"]"
	end,
    lists:flatten(lists:append([StringFunc,StringArgs,StringAnnots])).

