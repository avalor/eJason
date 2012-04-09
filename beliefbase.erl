-module(beliefbase).

-export([start/0,start/1,assert/2,find/2,deny/2,deny_matching/2]).


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
    case lists:member(Fact,BB) of
	true ->
	    {ok, no_change};
	false ->
	    {ok, [Fact|BB]}
    end;
assert(Fact,BB) when is_atom(Fact)->
    assert({Fact,{},[]},BB).



deny(Fact, BB) when is_tuple(Fact) ->
    case lists:member(Fact,BB) of
	true->
	    {ok,lists:delete(Fact,BB)};
	false->
	    {ok,no_change}
    end;
deny(Fact,BB) when is_atom(Fact)->
    deny({Fact},BB).


find(Query,BB) when is_tuple(Query)->
    QueryList = tuple_to_list (Query),
    FilterFun = fun (Fact) -> match_fact(QueryList,
					 tuple_to_list(Fact)) end, 
    lists:filter(FilterFun,BB).




match_fact([],[])->
    true;
match_fact([QueryTerm|QueryTerms],[Term|Terms]) when length(QueryTerms) ==
						     length(Terms)->
    case QueryTerm of
	'_' ->
	   match_fact(QueryTerms,Terms);
	Term ->
	    match_fact(QueryTerms,Terms);
	_ when is_tuple(QueryTerm) 
		  and is_tuple(Term) ->
	    case match_fact(
		   tuple_to_list(QueryTerm),
		   tuple_to_list(Term)) of
		true ->
		    match_fact(QueryTerms,Terms);				    		_->
		    false
	    end;
	_ ->
	    false
    end;
match_fact(_,_) ->
    false.

deny_matching({Atom,Params,Label},BB)->
    NewParams = lists:duplicate(size(Params),'_'),
    NewQuery = {Atom,list_to_tuple(NewParams),Label},    
    QueryList = tuple_to_list (NewQuery),
    FilterFun = fun (Fact) -> match_fact(QueryList,
					 tuple_to_list(Fact)) end, 
    Res = lists:dropwhile(FilterFun,BB),
   
   % io:format("DENY MATCHING: ~p~n for BB: ~p~nRes: ~p~n",
%	      [NewQuery,BB,Res]),
    Res.

