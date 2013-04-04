%-record(create_agent_request,
%	id,
	

-record(request,
	{type, %
	id = erlang:now(), % time stamp
	answerTo, % pid or container
	info % Contains the info of the request e.g. the agent created
%	updated = erlang:now()
	}).

-record(response,
	{type, %
	id = erlang:now(), % time stamp
	sentBy, % pid or container
	info, % Contains the info of the request e.g. the agent created
	 result,
	updated = erlang:now()
	}).

%% Info dependending on type:
%%
%%    type = create_agent : info = AgentName
%%
%%
%%
