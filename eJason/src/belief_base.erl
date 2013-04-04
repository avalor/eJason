-module(belief_base).

-export([start/0,start/1,assert/2,find/2,negate/2,negate_matching/2,
	query_bb/4, match_annotations/2, check_consistency/1]).

-include("variables.hrl"). %% NO OTHER RECORD SHOULD BE SENT HERE


start()->
    [].


start(BB) when is_tuple(BB)->
    [BB];
start(BB) when is_list(BB)-> 
    BB;
start(BB) when is_atom(BB)->
    [BB].

assert({Fact},BB) ->
    assert({Fact,{},[]},BB);
assert(Fact, BB) when is_tuple(Fact)->
%    io:format("Asserting ~p \nin ~p~n",[Fact,BB]),
% TODO:try to replace member 
    case lists:member(Fact,BB) of
	true ->
%	    io:format("No change\n"),
	    {ok, no_change};
	false ->
%	    io:format("NewBB: ~p~n",[ [Fact|BB]]),
	    {ok, [Fact|BB]}
    end;
assert(Fact,BB) when is_atom(Fact)->
    assert({Fact,{},[]},BB).




negate(Query, BB) when is_tuple(Query) ->
 %   io:format("Negate Query:~p~nBB: ~p~n",[Query,BB]),
   % QueryList = o_list(preprocess_query(Query)),
    %    io:format("Negate QueryList: ~p~n ",[QueryList]),

    drop_first(Query,BB,[]);
negate(Query,BB) when is_atom(Query)->
    negate({Query,{},[]},BB).


drop_first(_Query,[],_Acc)->
 % io:format("Nothing removed\n"),
    {ok, no_change};
drop_first(Query,[Fact|List],Acc)->
    case match_fact(Query, Fact) of
	true ->
	    Res = lists:reverse(Acc)++List,
%	    io:format("Removed. Res: ~p~n",[Res]),
	    {ok,Res};
	false->
	   
	    drop_first(Query,List,[Fact|Acc])
    end.



%% Finds which elements of BB match the pattern in Query
find(Query,BB)->
%    io:format("Queried: ~p~n",[Query]),
 %   io:format("BB: ~p~n",[BB]),
%    QueryList = tuple_to_list (Query),

    FilterFun = fun (Fact) -> match_fact(Query,
					 Fact) end, 
    Res = lists:filter(FilterFun,BB),
 %   io:format("Result from BB: ~p~n",[Res]),
    Res.




%match_fact({QueryName,QueryArgs,QueryAnnots}, 
%	   {FactName = #var{bind = ?UNBOUNDVAR},
%	    {},FactAnnots}) ->
%	    io:format("ASDADASD ~p~n",[QueryArgs]),
%    case QueryArgs of
%	'_' -> % To avoid a loop in this function matching
%	    io:format("ASDADASD ~n~n"),
%	    match_fact({QueryName,{},QueryAnnots},
%		       {FactName#var{bind = QueryName},{},FactAnnots});
%	_ ->
%	    match_fact({QueryName,QueryArgs,QueryAnnots},
%		       {FactName#var{bind = QueryName},QueryArgs,FactAnnots})
%   end;
match_fact(A,A) ->
    true;
match_fact({QueryName,'_',QueryAnnots},
	  {FactName,FactArgs,FactAnnots}) ->
    match_fact({QueryName,FactArgs,QueryAnnots},
	  {FactName,FactArgs,FactAnnots});

match_fact({QueryName,QueryArgs,QueryAnnots},
	   {FactName,FactArgs,FactAnnots}) when size(QueryArgs) ==
						size(FactArgs)->
 %   io:format("QName: ~p  FactName: ~p~n"++
%	  "QArgs: ~p~nFactArgs: ~p~n"++
%	  "QAnnots: ~p  FactAnnots: ~p~n",[QueryName,FactName,
%					      QueryArgs,FactArgs,QueryAnnots,
%					      FactAnnots]),
    NameMatch = 
	case {QueryName,FactName} of
	    {FactName,FactName} ->
		true;
	    {'_',_}->
		true;
%	    {_,#var{bind = ?UNBOUNDVAR}}-> % happens only in annots
%		    true;
%	    {FactMatch, #var{bind = FactMatch}}-> % happens only in annots
%		true;
	    _ ->
		false
		    end,

%   io:format("Name Match: ~p~n",[NameMatch]),
    if 
	NameMatch ->
	    
	    case match_args(tuple_to_list(QueryArgs),tuple_to_list(FactArgs)) of
		true ->
		    match_annots(QueryAnnots,FactAnnots);
		false ->
%		    io:format("args do not match~p"),
		    false
	    end;
	true ->
	    false
    end;
match_fact(A= {QueryName,QueryArgs,QueryAnnots},
	   B={FactName,FactArgs,FactAnnots} ) ->
%    io:format("~p does not match ~p~n",[A,B]),
%   io:format("Because:  size(QueryArgs) =/=
%    size(FactArgs) -> ~p =/= ~p~n  QueryName =/= FactName 
%    ~p =/= ~p~n",[ size(QueryArgs), size(FactArgs),QueryName,FactName]),

    false.


match_args([],[])->
    true;
match_args(SameArgs,SameArgs)->
    true;
match_args([QueryArg|RestQuery],[FactArg|RestFact])->
    
    case QueryArg of
	'_' ->
	   match_args(RestQuery,RestFact);
	FactArg ->
	   match_args(RestQuery,RestFact);
	{FactArg,{},[]}->
	    match_args(RestQuery,RestFact);
	
	_ when is_tuple(QueryArg) 
		  and is_tuple(FactArg) ->
	    case match_fact(QueryArg,FactArg) of
		true ->
		    match_args(RestQuery,RestFact);
		_->
		    false
	    end;
	_ ->
	    false
    end;
match_args(_,_) ->
    false.


%%%%%%%%%%%%%
% First elem: Annotations QueryTuple
% Second elemen: Annotations from a FactTuple

match_annots([],_)->
    true;
match_annots([QueryAnnot|Annots],FactAnnots) ->
    %io:format("Trying to match ~p in ~p~n",[QueryAnnot,FactAnnots]),
    case find(QueryAnnot,FactAnnots) of
	[] ->
	    false;
	_ ->
	    match_annots(Annots,FactAnnots)
    end.




negate_matching(ReceivedQuery = {Atom,Params,Label},BB)->
    NewParams = lists:duplicate(size(Params),'_'),
    Query = preprocess_query(ReceivedQuery),
 %   io:format("Query: ~p~n",[Query]),
    
    NegateQuery = setelement(2,Query, 
			    erlang: make_tuple(size(element(2,Query)),
					'_')),
  %  io:format("NegateQuery: ~p~n",[NegateQuery]),


%{Atom,list_to_tuple(NewParams),Label},    
   % QueryList = tuple_to_list (NegateQuery),
    FilterFun = fun (Fact) -> not match_fact(NegateQuery,
					 Fact) end, 
    Res = lists:filter(FilterFun,BB),
   %io:format("NEGATE MATCHING: ~p~n for BB: ~p~nRes: ~p~n",
%	      [Query,BB,Res]),
    Res.


%% Returns ALWAYS an iterator (which may be empty)
query_bb(BB,Bindings,Module, ReceivedQuery = {Name,Args,_Annot}) ->
 % io:format("Module: ~p~n",[Module]),

%		  io:format("QUERYBB Bindings: ~p~n",[Bindings]),
%		  io:format("QUERYBB Args: ~p~n",[Args]),


%		  io:format("ReceivedQuery: ~p~n",[ReceivedQuery]),

    Query =  %% change: {'_',{},[]} -> {'_','{}',[]}
	case preprocess_query(ReceivedQuery)of
	    {'_',{},[]} ->
		{'_','_',[]};
	    NewQuery ->
		NewQuery
	end,
%io:format("BB: ~p~n",[BB]),
	  
%io:format("Preprocessed Query: ~p~n",[Query]),

    BeliefResults = find(Query,BB),
%    io:format("Belief results: ~p~n",[BeliefResults]),
%   NoBeliefResults = 
%	case BeliefResults of 
%	    [] ->
%		true;
%	    _ ->
%		false
%	end,
    
    ItBeliefs = iterator:create_iterator(BeliefResults),    


    
    Arguments = [BB]++[[]]++variables:fully_valuate(tuple_to_list(Args)),
		  
	%	io:format("Arguments: ~p~n",[Arguments]),

    FinalIterator = 
	try


%% TODO: consider the case the NAME is a variable (anonym rule)

%	    io:format("Calling ~p:~p(~p)\n\n\n\n\n\n",
%		      [Module,Name,Arguments]),

	    case apply(Module,Name,Arguments) of
%		false ->
%		  io:format("FALSE ~n"),
%		    if
%			NoBeliefResults ->
%			    false;
%			true ->
%			    ItBeliefs
%		    end;
 %A RULE ALWAYS RETURNS AN ITERATOR
		Iterator when is_function(Iterator) ->
%		    case Iterator() of
%			false -> %% Rule functions does not match
%			    if
%				NoBeliefResults ->
%				    false;
%				true ->
%				    ItBeliefs
%			    end;
%                      _ ->
%		    if
%			NoBeliefResults -> 
				    
%				    io:format("All Values: ~p~n",
%				      [iterator:get_all(Iterator)]),
%			    Iterator;
%			true ->
			    
			    Concat = iterator:concat(ItBeliefs,Iterator),
			 %   io:format("All Values: ~p~n",
			%	      [iterator:get_all(Iterator)]),
			    Concat
%		    end 
	    end
	catch 
	    error:undef -> %% No rule associated to the predicate
	%	io:format("UNDEF\n"),

			ItBeliefs;
	     error:badarg -> %% 
	%	io:format("BADARG\n"),
			ItBeliefs
	end,
%io:format("FinalIterator: ~p~n",[iterator:get_all(FinalIterator)]),


    %% The return value is an iterator that provides matches
    %% for the variables in the query 


  %  case FinalIterator of
%	false->
%	    false;
%	_ when is_function(FinalIterator) ->

	    Fun = fun (Result) ->
%		  io:format("ReceivedQuery: ~p~n",[ReceivedQuery]),

%		  io:format("Query: ~p~n",[Query]),

%		  io:format("Query Result: ~p~n",[Result]),

		  case Result of
		      {Params, Valuation}-> %% Result from a rule
                % io:format("ReceivedQuery: ~p~n",[ReceivedQuery]),
%			  io:format("Unvaluated Vars: ~p~n",[Args]),
%			  io:format("Unvaluated Params: ~p~n",[Params]),

			  ValuatedVars =
			      variables:valuate_return_params(Params,
							      Valuation), 

%			  io:format("ValuatedVars: ~p~n",
%				    [ValuatedVars]),
			  MatchFun = 
			      fun ({Var1,Var2}) ->
				      variables:match_vars(Var1,
							   Var2) 
			      end,
			  Matched =  lists:map(MatchFun,
					   lists:zip(tuple_to_list(Args),
						     ValuatedVars)),
			  
			  
%			  io:format("BB Matches: ~p~n",[Matched]),
			   Res = variables:retain_unique_vars(Matched),
%			  io:format("BB Results: ~p~n",[Res]),
			  Res,
			  
						 


%		 io:format("From Rule: ~p~n",[A]),
			  iterator:create_iterator(
			    [Res]);
			   %++Valuation]);
	
		      {_,_,_}-> %%{Params,Args,Annot}: Result from a belief
			  
		       case postprocess_query(Result,ReceivedQuery) of
			   false ->
			       false;
			   Vars when is_list(Vars) ->
			%   io:format("ReceivedQuery: ~p~n",[ReceivedQuery]),

%		       io:format("PostProcessquery: ~p~n",[Vars]),
			       iterator:create_iterator(
				    [Vars])
		       end;	      
		      
		      Other->
			  io:format("[belief_base] Expected a tuple,"++ 
				    "found: ~p~n",[Other])
		  end
	  end,

	    iterator:create_iterator_fun(FinalIterator,Fun).
%    end.
    



%% Queries may receive variable records but
%% the BB does not contain any. Thus, the query
%% shall be preprocessed.

%preprocess_query( {Name,{}, []})->
%    preprocess_query(Name);
preprocess_query({Name = #var{},{}, []})->  % happens in annotations
    %io:format("Name: ~p~n",[Name]),
    case preprocess_query(Name) of
	'_' ->
	    {'_','_',[]};
	Else->
	    Else
    end;	    
preprocess_query( T={Name,Args, Annot})->
    %io:format("Tuple: ~p\n",[T]),

    {case preprocess_query(Name) of % Added to handle annot matches
	 {AtomName,{},[]} when is_atom(AtomName)->
	     AtomName;
	 Other -> Other
     end,
     list_to_tuple(preprocess_query(tuple_to_list(Args))),
     preprocess_query(Annot)};
preprocess_query(List) when is_list(List) ->
    %io:format("List: ~p~n",[List]),
    Fun = fun (X) -> 
		  preprocess_query(X) end,
    lists:map(Fun, List);
preprocess_query(#var{bind = Bind})->
   % io:format("Bind: ~p~n",[Bind]),
    case Bind of
	{} ->
	    '_';
	Atom when is_atom(Atom) ->
	    {Atom,{},[]};
	_ ->
	    preprocess_query(Bind)
    end;
preprocess_query(Tuple) when is_tuple(Tuple) ->
    %io:format("List: ~p~n",[Tuple]),
    list_to_tuple(preprocess_query(tuple_to_list(Tuple)));

preprocess_query(Atom) when is_atom(Atom)->
   % io:format("Atom\n"),
   Atom;
preprocess_query(Number) when is_number(Number)->
    %io:format("Atom\n"),
    Number.
%preprocess_query(Otro) ->
 %   io:format("Otro: ~p~n",[Otro]).





%% Matches all variables in the query to
%% the values in the result.
%% Returns a list of variable matches or
%% false if some variable could not be matched.
postprocess_query(Result =
		  {_NameR,_ArgsR,_AnnotR},
		  {Var=#var{bind={}}, 
			% query is a stand-alone variable
		   {},[]})->
    [Var#var{bind =
	     variables:fully_valuate(Result)}];   
	
postprocess_query(Result = 
		  {NameR,ArgsR,AnnotR},
		  Query =
		  {NameQ,ArgsQ,AnnotQ}) ->

    %io:format("Postprocessquery->\n\tQuery: ~p~nResult: ~p~n",[Query,Result]),
    


    PreRes = {postprocess_query(NameR,NameQ),
	      list_to_tuple(
		postprocess_query(
		  tuple_to_list(ArgsR),
		  tuple_to_list(ArgsQ))),
	     postprocess_query(AnnotR,AnnotQ)},
    Matches = variables:retain_variables(PreRes),
%    io:format("Matches: ~p~n",[Matches]),
    case check_consistency(Matches) of
      false ->
	  false;
      Vars ->
	  utils:erase_repeated_vars(Vars)
  end;
postprocess_query(Value, Var = #var{})->
    Var#var{bind = Value, is_ground = true};
postprocess_query(Atom, Atom) ->
    Atom;
% Postprocessing annotations. TODO: It must change to consider different lenghts
% and match sources properly
postprocess_query(List1,[]) when is_list(List1)->
    List1;
postprocess_query(List1,List2) when is_list(List1),
				    is_list(List2)->
    List3 = [X ||
		X <- lists:zip(List1,List2)],
    Fun = fun ({Value1,Value2}) ->
		  postprocess_query(Value1,Value2) end,
    lists:map(Fun,
	     List3).

    


%% Checks whether a valuation does is consistent
%% or not, e.g. X = Y and X = Z iff Y = Z
%% Returns either the valuation or "false".
check_consistency(List)->
%    io:format("Consistency List: ~p~n",[List]),
    check_consistency(List,[]).


check_consistency([],Acc)->
    Acc;
check_consistency([Var|Vars],Acc)->
    Fun = fun(X) ->
		  is_consistent(Var,X) end,
    Res = lists:map(Fun,Vars),
    case lists:member(false,Res) of
	true ->
	    false;
	false ->
	    check_consistency(Vars,[Var|Acc])
    end.


is_consistent(Var = #var{timestamp = TS, bind = Bind1},
	   #var{timestamp = TS,bind = Bind2})->
    case Bind1 == Bind2 of
	false ->
	    false;
	true ->
	    Var
    end;
is_consistent(Var,_) ->
    Var.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% FUNCTIONS DEALING WITH ANNOTATIONS
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% Matches the variables in Annotations and Query
%% Annotations comes from runtime execution. Query comes from source
%% code
%% Returns ALWAYS an iterator
match_annotations(Annotations, Query) ->
%   io:format("[MATCH ANNOTATION]:\nAnnotations: ~p~nQuery: ~p~n",
%	      [Annotations,Query]),
    try
	It = query_annotations(Annotations, Query),
	
%	io:format("Matched annotations with: ~p~n",[iterator:get_all(It)]),

	It
    catch
	A:B ->
	    io:format("Error ~p:~p in Match Annotation~n",[A,B]),
	    {iterator:create_iterator([]),[]} 
    end.



%% Returns ALWAYS an iterator (which may be empty)
query_annotations(Annotations,ReceivedQuery = {Name,Args,_Annot}) ->
   
 %   io:format("Received query: ~p~n",[ReceivedQuery]),
    
    Query =  %% change: {'_',{},[]} -> {'_','{}',[]}
	case preprocess_query(ReceivedQuery)of
	    {'_',{},[]} -> % For standalone variables in annot
		{'_','_',[]};
	    {{QAtom,{},[]},QArgs,QAnnots} when is_atom(QAtom),
					       is_tuple(QArgs),
					       is_list(QAnnots)->
		{QAtom,QArgs,QAnnots};
	    NewQuery ->
		%io:format("Did not match: ~p~n",[NewQuery]),
		NewQuery
	end, 

%io:format("preprocessed annot query: ~p~n",[Query]),
    BeliefResults = find_annot(Annotations,Query),
    
    ItBeliefs = iterator:create_iterator(BeliefResults),    

%io:format("~n~n~nPOSTPROCESSING: ~p~n~n",[BeliefResults]),

    PostFun = 
	fun (Result) ->
%		io:format("Postprocessing Result: ~p~nReceivedQuery: ~p~n",
%			  [Result,ReceivedQuery]),
		case postprocess_annot_match(Result,
					     ReceivedQuery) of
		    false->
			false;
		    PreRes->
			Matches = variables:retain_variables(PreRes),
			case check_consistency(Matches) of
			    false ->
				false;
			    Vars ->
				[utils:erase_repeated_vars(Vars)]
			end
%      io:format("Result of postprocessing annots: ~p~n",
%					[Matches]),
		end 
	end,
    
    iterator:create_iterator_fun(ItBeliefs,PostFun).
    
			      
			      
     
%    Arguments = [[]]++variables:fully_valuate(tuple_to_list(Args)),
		  
%    io:format("Arguments: ~p~n",[Arguments]),
    


%% Finds which elements of the annotations match the pattern in Query
%% Returns a list with those elements.
find_annot(Annotations,Query)->
%    io:format("Queried: ~p~n",[Query]),
%    io:format("Annotations: ~p~n",[Annotations]),
    QueryList = tuple_to_list (Query),

    FilterFun = fun (Annot) -> match_annot(Query,
					   Annot) end, 
    Res = lists:filter(FilterFun,Annotations),
%    io:format("Result from Annotations: ~p~n",[Res]),
    Res.


match_annot({QueryName,QueryArgs,QueryAnnots}, 
	   {AnnotName = #var{bind = ?UNBOUNDVAR},
	    {},[]}) ->
    case QueryArgs of
	'_' -> % To avoid a loop in this function matching
	    match_annot({QueryName,{},QueryAnnots},
		       {AnnotName#var{bind = QueryName},{},QueryAnnots});
	_ ->
	    match_annot({QueryName,QueryArgs,QueryAnnots},
		       {AnnotName#var{bind = QueryName},QueryArgs,QueryAnnots})
    end;

match_annot({QueryName,QueryArgs,QueryAnnots}, 
	   {AnnotName = #var{bind = ?UNBOUNDVAR},
	    {},AnnotAnnots}) ->
    case QueryArgs of
	'_' -> % To avoid a loop in this function matching
	    match_annot({QueryName,{},QueryAnnots},
		       {AnnotName#var{bind = QueryName},{},AnnotAnnots});
	_ ->
	    match_annot({QueryName,QueryArgs,QueryAnnots},
		       {AnnotName#var{bind = QueryName},QueryArgs,AnnotAnnots})
    end;
match_annot({QueryName,'_',QueryAnnots},
	  {AnnotName,AnnotArgs,AnnotAnnots}) ->
    match_annot({QueryName,AnnotArgs,QueryAnnots},
	  {AnnotName,AnnotArgs,AnnotAnnots});
match_annot({QueryName,QueryArgs,QueryAnnots},
	   {AnnotName,AnnotArgs,AnnotAnnots}) when size(QueryArgs) ==
						size(AnnotArgs)->
  %  io:format("QName: ~p  AnnotName: ~p~n"++
%	  "QArgs: ~p~nAnnotArgs: ~p~n"++
%	  "QAnnots: ~p  AnnotAnnots: ~p~n",[QueryName,AnnotName,
%					      QueryArgs,AnnotArgs,QueryAnnots,
%					      AnnotAnnots]),
    NameMatch = 
	case {QueryName,AnnotName} of
	    {AnnotName,AnnotName} ->
		true;
	    {'_',_}->
		true;
	    {_,#var{bind = ?UNBOUNDVAR}}-> 
		    true;
	    {AnnotMatch, #var{bind = AnnotMatch}}-> 
		true;
	    _ ->
		false
		    end,

%   io:format("Name Match: ~p~n",[NameMatch]),
    if 
	NameMatch ->
	    
	    case match_args(tuple_to_list(QueryArgs),tuple_to_list(AnnotArgs)) of
		true ->
		    match_annots(QueryAnnots,AnnotAnnots);
		false ->
%		    io:format("args do not match~p"),
		    false
	    end;
	true ->
	    false
    end;
match_annot(A= {QueryName,QueryArgs,QueryAnnots},
	   B={AnnotName,AnnotArgs,AnnotAnnots} ) ->
%    io:format("[BB:matchannot, info]~p does not match ~p~n",[A,B]),
%   io:format("Because:  size(QueryArgs) =/=
%    size(AnnotArgs) -> ~p =/= ~p~n  QueryName =/= AnnotName 
%    ~p =/= ~p~n",[ size(QueryArgs), size(AnnotArgs),QueryName,AnnotName]),
    false.



%% Matches all variables in the query and result.
%% Returns a list of variable matches or
%% false if some variable could not be matched.
postprocess_annot_match(
			{Var=#var{bind=?UNBOUNDVAR}, {},[]},
		        {QName=#var{bind=?UNBOUNDVAR}, {},[]})->
                         % Result and query are stand-alone variables
    [Var#var{bind = QName#var.timestamp}];   
postprocess_annot_match(_Result =
			{Var=#var{bind={}}, {},[]},
		       Query =
		        {_QName, _QArgs,_QAnnots})->
                         % Result is a stand-alone variable

    [Var#var{bind = variables:make_timestamp_tuple(Query)}];   

postprocess_annot_match(Result =
		  {_NameR,_ArgsR,_AnnotR},
		  {Var=#var{bind={}},  {},[]})->


       % Query is a stand-alone variable
    [Var#var{bind =
	     variables:fully_valuate(Result)}];   

postprocess_annot_match(Result = 
		  {NameR,ArgsR,AnnotR},
		  Query =
		  {NameQ,ArgsQ,AnnotQ}) ->

%    io:format("PostprocessqueryAnnot->\n\tQuery: ~p~nResult: ~p~n",
%	      [Query,Result]),
    
    PreRes = {postprocess_annot_match(NameR,NameQ),
	      list_to_tuple(
		postprocess_annot_match(
		  tuple_to_list(ArgsR),
		  tuple_to_list(ArgsQ))),
	     postprocess_annot_match(AnnotR,AnnotQ)},
    Matches = variables:retain_variables(PreRes),
    case check_consistency(Matches) of
      false ->
	  false;
      Vars ->
	  utils:erase_repeated_vars(Vars)
  end;
postprocess_annot_match(Value, Var = #var{})->
    Var#var{bind = Value, is_ground = true};
postprocess_annot_match(Atom, Atom) ->
    Atom;
postprocess_annot_match(List1,List2) when is_list(List1),
				    is_list(List2)->
    List3 = [X ||
		X <- lists:zip(List1,List2)],
    Fun = fun ({Value1,Value2}) ->
		  postprocess_annot_match(Value1,Value2) end,
    lists:map(Fun,
	     List3).

    










