-record(send_message_response, %% From DM to agent
	{id = no_id_given,
	 result = no_result, %% ?AGENTFOUND, ?NOAGENT
	 sent_by = node(),
	 agent_name = no_name,
	 found_in_container = no_found_in_container}).



-record(create_agent_response, %% From DM to agent
	{id = no_id_given,
	 result = no_result, %% ?NAMECLASH, ?CREATED, ?NOCODE,  %% ?NOCONTAINER
	 sent_by = node(),
	 agent_name = no_name,
	 container = no_container,
	 code = no_code}).


-record(is_agent_response, %% From DM to DM
	{id = no_id_given,
	 result = no_result, %% ?AGENTFOUND,?NOAGENT
	 sent_by = node(),
	 agent_pid = no_pid,
	 agent_name = no_name}).

-record(agent_list_response, %% DM to DM
	{id = no_id_given,
	 sent_by = node(),
	 agent_list = []}).

-record(connection_response, %% From DM to agent
	{id = no_id_given,
	 result = no_result, %% ?CONNECTED,?NOCONTAINER, ?NAMECLASH
	 sent_by = node(),
	 parent_task = no_parent,
	 conflicting_agents = [],%% Only varies if result = ?NAMECLASH
	 container = no_container}).

-record(disconnection_response, %% From DM to agent
	{id = no_id_given,
	 sent_by = node()}).


-record(check_agents_response, %% From initiator DM to receiver DM
	{id = no_id_given,
	 sent_by = node(),
	 agents = no_agent_list}).


-record(system_connection_response, %% From receiver DM to initiator DM
	{id = no_id_given,
	 sent_by = node(),
	 agents_in_receiver_mas = no_agents_in_receiver, 
	 %% necessary to send the proper agent_up notifications
	 result = no_result, %% ?EJASONOK, ?NAMECLASH
	 conflicting_agents = [],%% Only varies if result = ?NAMECLASH
	 containers = no_containers}).%% containers in local MAS


-record(connect_to_response, %% DM to initiator DM
	{id = no_id_given,
	 sent_by = node()}).


-record(get_info_response, %%  DM to agent
	{id = no_id_given,
	 agents = no_agents,
	 containers = no_containers,
	 reserved_names = no_reserved_names,
	 sent_by = node()}).

-record(system_disconnection_response, %% DM to DM
	{id = no_id_given,
	 sent_by = node()}).
