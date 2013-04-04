-module(private_query).

-export([resolve_query/2, formula/1]).

-include("parser.hrl").
-include("variables.hrl").
-include("macros.hrl").
-include("ejason.hrl").


resolve_query(_Agent, 
	   {my_name, add_ejason_private_query,_ModuleName,
	    {Var},_Annot}) ->%when is_record(Var,var)->
    
    try
	MyName = case process_info(self(),registered_name) of
		     [] ->
			 io:format("[Private_query] Unregistered agent~n"),
			 exit(fatal_error);
		     {registered_name,AgentName}->
			 AgentName
		 end,

	PrivateGoal = #var{is_ground = true, bind = MyName},
%	io:format("Var: ~p~n",[Var]),
	case Var of
	    Atom when is_atom(Atom)->
		Result =  variables:match_vars(PrivateGoal,Atom),		
		{[Result],[]};

	    {Atom,{},[]} when is_atom(Atom)->
		Result =  variables:match_vars(PrivateGoal,Atom),
			 
		{[Result],[]};
	    _ when is_record(Var,var) ->
		Result =  variables:match_vars(PrivateGoal,Var),
		{[Result],[Var#var{bind = Result#var.timestamp}]};
	    _ ->
		false	
	end
	
    catch
	exit:param_does_not_match ->
	    false
    end;
resolve_query(_Agent, 
	   {my_container, add_ejason_private_query,_ModuleName,
	    {Var},_Annot}) when is_record(Var,var)->
    
    try
	PrivateGoal = #var{is_ground = true, bind = node()},
	
	Result =  variables:match_vars(PrivateGoal,Var),
	{[Result],[Var#var{bind = Result#var.timestamp}]}
	
    catch
	exit:param_does_not_match ->
	    false
    end;



resolve_query(Agent, 
	   {my_environment, add_ejason_private_query,_ModuleName,
	    {Var},_Annot}) when is_record(Var,var)->
    
    try
	PrivateGoal = #var{is_ground = true, bind = 
			   Agent#agentRationale.environment},
	
	Result =  variables:match_vars(PrivateGoal,Var),
	{[Result],[Var#var{bind = Result#var.timestamp}]}
	
    catch
	exit:param_does_not_match ->
	    false
    end;
resolve_query(_Agent,Query) ->
    io:format("No match for private_query: ~p~n",[Query]),
    false.

formula(_Valuation) ->
%    io:format("TESTGOAL RESULT: ~p~n",[Valuation]),
    {stutter_action}.



						  

