-record(send_message_request, %% Agent to DM
	{id = erlang:now(),
	 answerTo = no_answer_to,
	 agent_name = no_name,
	 suggested_container = no_suggested_container}).
	 %% performative = no_performative,
	 %% message = no_message}).


-record(create_agent_request, %% Agent to DM
	{id = erlang:now(),
	 answerTo = no_answer_to,
	 agent_name = no_name,
	 container = no_container,
	 code = no_code}).


-record(is_agent_request, %% DM to DM
	{id = erlang:now(),
	 answerTo = no_answer_to,
	 agent_name = no_name}).

-record(connection_request, %% agent to DM
	{id = erlang:now(),
	 answerTo = no_answer_to,
	 container = no_container,
	 parent_task = no_parent}).

-record(system_connection_request, %% DM to DM
	{id = erlang:now(),
	 answerTo = no_answer_to}).

-record(disconnection_request,
	{id = erlang:now(),
	 answerTo = no_answer_to,
	 container = no_container}).

-record(agent_list_request, %% DM to DM
	{id = erlang:now(),
	 answerTo = no_answer_to}).

-record(connect_to_request, %% DM to DM
	{id = erlang:now(),
	 answerTo = no_answer_to,
	 containers = no_containers}).

-record(check_agents_request, %% from receiver DM to initiator DM
	{id = erlang:now(),
	 answerTo = no_answer_to
	}).

-record(get_info_request, %% agent to DM 
	{id = erlang:now(),
	 answerTo = no_answer_to
	}). 

-record(print_info_request, %% Erlang shell to DM
	{id = erlang:now(),
	 answerTo = no_answer_to}).

-record(system_disconnection_request, %% DM to DM
	{id = no_id_given,
	 answerTo = no_answer_to,
	 container = no_disconnect_container}).


%% -record(request,
%% 	{type, %
%% 	id = erlang:now(), % time stamp
%% 	answerTo, % pid or container
%% 	info % Contains the info of the request e.g. the agent created
%% %	updated = erlang:now()
%% 	}).




%% %%% Responses are typically sent by the DM and received by an agent ***
%% -record(response,
%% 	{type, %
%% 	id = erlang:now(), % time stamp
%% 	sentBy, % pid or container
%% 	info, % Contains the info of the request e.g. the agent created
%% 	 result,
%% 	updated = erlang:now()
%% 	}).

