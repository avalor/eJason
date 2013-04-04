-module(environment_handler).

-export([execute/6,is_string/1,execute_internal/2]).

-include("variables.hrl").
-include("macros.hrl").


%% Executes the external function ActionName implemented in the
%% module PackageName
%% Returns false if the action could not be executed or a tuple
%% {ok, NewMatches} where NewMatches are the bindings for variables 
%% in the parameters taken from the result of the execution of the function
execute(EnvironmentName,ActionName,ActionParams,MatchParams,
	ResponseAgent,TimeStamp)->
    try

%       Execute the action

%	{ActionParams,MatchParams} = split_params(Params),
%	io:format("Matchings: ~p~n",[MatchParams]),
%	io:format("Params: ~p~n",[ActionParams]),

	Result = apply(EnvironmentName,ActionName,ActionParams),
%	io:format("Ext Act Result: ~p~n",[Result]),
	    % Parse the result and return the proper answer
	ResponseAgent ! {external_action,{self(),TimeStamp},
			 parse_result(Result,MatchParams)}
    catch _A:_B ->
%	    io:format("ERROR executing external function: ~p:~p~n",[A,B]),
	    ResponseAgent ! {external_action,{self(),TimeStamp},false}
    end.



execute_internal(PackageList,{ActionName,Params,MatchingParams})->
    try
	case PackageList of 
	    %io:format("Params: ~p~n",[
	    [Module]->
		Result =
		    apply(Module,ActionName,tuple_to_list(Params)),
%		io:format("Ext Act Result: ~p~n",[Result]),

		case Result of
		    {?EJASONOK,ResultValues,ParamsToMatch}-> % user-defined InAc
			parse_result(ResultValues,
				     ParamsToMatch);
		    _ ->
			parse_result(Result,MatchingParams)
		end;     
	    _ ->
		    io:format("Cannot execute PackageList ~p. Java interface"++
			      " not yet implemented.~n",[PackageList]),
		{fail}
	end
    catch
	A:B ->
	    io:format("ERROR executing internal function: ~p:~p~n",[A,B]),
	    {fail}
    end.
	
	

			      

%% Boolean function that returns true if the term given is a list of
%% integers.
is_string([])->		  
    true;
is_string([Int|Rest]) when is_integer(Int)->
    is_string(Rest);
is_string(_) ->
    false.


parse_result(Values,MatchParams)->
    ValueList =
	case Values of
	    _ when is_tuple(Values) ->
		case tuple_to_list(Values) of
		    [ok|Rest] ->
			Rest;
		    [error|_Rest] ->
			false;
		    Other ->
			Other
		end;
	    _  ->%is_list(Values) ->
		[Values]%;
%	    _ ->
%		[Values]
	end,
    TypedValues =
	lists:map(fun change_type/1,ValueList),
    match_result(MatchParams,TypedValues).


change_type(List) when is_list(List) ->
    lists:map(fun change_type/1,List);
change_type(Tuple) when is_tuple(Tuple) ->
    change_type(tuple_to_list(Tuple));
change_type(Binary) when is_binary(Binary)->
    binary_to_list(Binary);
change_type(A) ->
    A.




% Binds the unbound variables in the parameters of the function
% to the results obtained through the execution of the external action.
match_result(MatchParams,ResultList)->
%    io:format("Match: ~p~nResultstomatch: ~p~n",[MatchParams,ResultList]),
    match_result(MatchParams,ResultList,[]).

match_result([],_ResultList,Accum)->
    {ok,Accum};
match_result([_|_],[],Accum) ->
    io:format("[environment_handler,warning]: more params to be matched than values returned by the function.~n"),
    {ok, Accum};
match_result([Param=#var{}|MatchParams],[Result|ResultList],Accum)->
 %   io:format("Resultado: ~p~n",[Result]),
    NewParam = 
		Param#var{bind = Result,is_ground=true},
    match_result(MatchParams,ResultList,[NewParam|Accum]);

match_result([{Var=#var{},{},[]}|Rest],ResultList,Accum) ->
    match_result([Var|Rest],ResultList,Accum).


%% Returns a tuple {ActionParams,MatchParams} where
%% ActionParams is a list with the parameters given to the action
%% MatchParams will be all those that follow the first unbound variable
%% MatchParams is a list with the variables to be bound to the result
%% of the execution of the action
split_params(Params) when is_tuple(Params)->
    split_params(tuple_to_list(Params),[]);
split_params(Params) ->
    split_params(Params,[]).


split_params([],Accum)->
    {lists:reverse(Accum),[]};
split_params(MatchParams = [#var{bind = ?UNBOUNDVAR}|_],Accum) ->
    {lists:reverse(Accum),MatchParams};
split_params([Other|Rest],Accum) ->
    split_params(Rest,[Other|Accum]).




%% Turns all tuples into lists. Jason does not recognize tuples, but it
%% does lists
%tuples_to_list(List) when is_list(List) ->
%    Fun = fun (X) ->
%		  tuples_to_list(X) end,
%    lists:map(Fun,List);
%tuples_to_list(Tuple) when is_tuple(Tuple) ->
%    tuples_to_list(tuple_to_list(Tuple));
%tuples_to_list(A) ->
%    A.























%execute(PackageName,ActionName,Params,ExecutionMode,ResponseAgent)->
%    try
%	case ExecutionMode of
%	    ?SPAWNPROCESS ->
%		spawn(environment_handler,execute,
%		      [PackageName,
%		       ActionName,
%		       Params,
%		       ?NOSPAWNPROCESS,
%		       self()]);
%	    ?NOSPAWNPROCESS ->
%		%Execute the action
%		{ActionParams,MatchParams} = split_params(Params),
%		Result = apply(PackageName,ActionName,ActionParams),
%	    % Parse the result and return the proper answer
%		ResponseAgent ! {external_action,
%				 parse_result(Result,MatchParams)}
%	end
%    catch _:_ ->
%	    ResponseAgent ! {external_action,false}
%    end.


%parse_result(ok,_)->
%    {ok,;

% Generates a tuple 
%parse_result(_,[])->% No new variables bindings
%    {ok,[]};
%parse_result(ResultList,MatchParams) when is_list(ResultList)->
 %   io:format("Result: ~p~n",[Result]),
 %   ResultList = tuple_to_list(Result),
%    io:format("Result List: ~p~n",[ResultList]),
%case ResultList of
%	[ok|Rest] ->
%	    match_result(MatchParams,Rest);
%	[error|_Rest] ->
%	    false;
%	_Other ->
%	    match_result(MatchParams,ResultList)
 %   end;
%parse_result(A,MatchParams) ->
%    parse_result([A],MatchParams).




%parse_result(ok,_) ->
%    {ok,[]};
%parse_result(Other,MatchParams) ->
 %   parse_result(Other,MatchParams).

