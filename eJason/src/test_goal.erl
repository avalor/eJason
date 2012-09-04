-module(test_goal).

-export([trigger/6,context/6, formula/1]).

-include("parser.hrl").
-include("variables.hrl").
-include("macros.hrl").


trigger(BB,Bindings0,#var{bind = PName},
		  Ejason_EjasonEventType,ModuleName,Args) ->
    try
%	io:format(" Ejason_EjasonEventType: ~p~n",
%		  [ Ejason_EjasonEventType]),
	TestGoal = #var{is_ground = true, bind = add_test_goal},
	
	Bindings1 = [TestGoal|Bindings0],
%	io:format("Bindings1: ~p~n",[Bindings1]),
	variables:try_param_match(Bindings1,TestGoal#var.timestamp,
				  Ejason_EjasonEventType),
	%% TODO consider the case for anonymous test_goals
%	io:format("test_goal_args: ~p~n",[Args]),
	Params =utils:erase_repeated_vars( variables:retain_variables(Args)),
	BoundParams = 
	     variables:get_ground_variables(Params),
%	io:format("Params for test goal: ~p~n",[Params]),
%	io:format("Args for test goal: ~p~n",[Args]),


%	io:format("OUT!!!\n\n\n\n"),
	Fun = fun (X) -> 
		      %io:format("test_goal X: ~p~n",[X]),
		      ContextBindings = X++BoundParams,		    
		      test_goal:context(BB,Params,ModuleName,
					   ContextBindings,PName,Args) end,
	ItList = iterator:create_iterator([Bindings1]),
	iterator:create_iterator_fun(ItList,Fun)

	catch
		exit:param_does_not_match ->
			false
	end;
trigger(_BB,_,_,_,_ModuleName,_) ->
    false.

context(BB,Params,ModuleName,Bindings,PName,Args)->
  % io:format("Context Bindings: ~p~n",[Bindings]),
    %io:format("Context Args: ~p~n",[Args]),

   case belief_base:query_bb(BB,Bindings,
			     ModuleName,{PName,Args,[]}) of 
       false->
	  % io:format("Test Goal Context: FALSE!\n"),
	   false;
       It when is_function(It) ->
	   ListParams =  
	       lists:map(fun (V = #var{timestamp = TS}) ->
				 V#var{bind = TS} end,
			 Params),
	   
	   Fun = fun (X) ->
			% io:format("X: ~p~n",[X]),

			 {ListParams,
			  X}

		%	 io:format("X: ~p~n",[X]),
			 end,
	   
	   %Fun = fun (X) ->
			 %io:format("X: ~p~n",[X]),
	%		 X end,
	   iterator:create_iterator_fun(It,Fun)
   end.

formula(Valuation) ->
%    io:format("TESTGOAL RESULT: ~p~n",[Valuation]),
    {stutter_action}.



						  

