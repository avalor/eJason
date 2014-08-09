-module(iterator).

-export([create_iterator/1,create_iterator_fun/2,
	 create_iterator_orddict/1,
	concat/2,get_all/1, first/1,get_some/2,
	create_recursive_iterator/2,
	create_recursive_iterator/3, combine/2, pile/1,
	any/2]).



%% Creates iterator for a list of values
create_iterator(List) when is_list(List) ->
  %  io:format("Iterator for List: ~p~n",[List]),
    fun () ->
	    iterator(List) end;
create_iterator(Other) ->
    create_iterator([Other]).
 

iterator([]) ->
    false;
iterator([Elem|List]) ->
    {Elem,
     fun () -> iterator(List) end}.



%% Creates an iterator for the values in an orddict
create_iterator_orddict(Orddict) ->
    fun () ->
	    iterator_orddict(Orddict) end.

iterator_orddict([]) ->
    false;
iterator_orddict([{_Key,Value}|Rest]) ->
    {Value,
     fun () -> iterator_orddict(Rest) end}.






% Constructs an iterator that returns a list with the results of
% of applying Fun recursively to some value
create_recursive_iterator(InitValue,Fun)->
    try
	fun () ->
		{InitValue,
		 create_recursive_iterator(Fun(InitValue),Fun)} end
    catch
	_:_->
	    false
    end.

% Constructs an iterator that returns a list with the results of
% of applying Fun recursively that fulfil Condition
create_recursive_iterator(InitValue,Fun,Condition)->
    try
	fun () ->
		case Condition(InitValue) of
		    true ->
			{InitValue,
			 create_recursive_iterator(Fun(InitValue),Fun,
						   Condition)};
		    _ ->
			false
		end
	end
    catch
	_:_->
	    false
    end.




%% If Fun returns an iterator, it is used to return several values
%% If Fun returns a list, its values are returned in subsequent calls
%% If Fun returns false, for some value, ONLY that value is omitted
create_iterator_fun(ItList,Fun) when is_function(ItList) ->
    fun () ->
	    iterator_fun(ItList,Fun,fun()->false end) end;
create_iterator_fun(List, Fun) when is_list(List)->
    ItList = create_iterator(List),
    create_iterator_fun(ItList,Fun).




%% Fun::=  [#var] -> iterator_fun() || [term()] || term()
iterator_fun(ItList,Fun,ChildIterator) -> 
   case ChildIterator() of
       false ->
	   case ItList() of
	       
	       false ->
		   false;
	       {Elem, NewItList} ->
%		  io:format("NewElem: ~p~n",[Elem]),
		   case apply(Fun,[Elem]) of
		       NewChildIterator when is_function(NewChildIterator) ->
			   NewItFun =    
			       fun () -> 
				       iterator_fun(
					 NewItList,Fun,NewChildIterator)
			       end,
			   NewItFun();
		       false ->
		%	   io:format("NewItList devuelve: ~p~n",
		%		     [NewItList()]),
			   iterator_fun(NewItList,Fun,ChildIterator);
		       A ->
			   NewIt = create_iterator(A),
			   {Val,NewChildIterator} = NewIt(),
			   {Val,
			    fun () -> iterator_fun(
					NewItList,Fun,NewChildIterator)end}
		   end
	   end;
       {Value,
        NewChildIterator}->
	   {Value,
	    fun () -> iterator_fun(ItList,Fun,NewChildIterator) end};	    
       A ->
	   io:format("[Iterator]Not dealt with: ~p~n",[A])
   end.
      

concat(It1,It2) ->
    ItIt = create_iterator([It1,It2]),
    Fun = fun (X) ->
		  X end,
    create_iterator_fun(ItIt,Fun).



%% WARNING! This can incur in an infinite loop in the case of an
%% infinite recursive iterator. Use "get_some" instead.
get_all(It)->
    get_all(It,[]).

get_all(It,Acc)->
    case It() of
	false ->
	    lists:reverse(Acc);
	{Elem,	NewIt} ->
	    get_all(NewIt,[Elem|Acc])
    end.


first(It)->
    case It() of
	{First,_}->
	    First;
	false ->
	    false
    end.

%% Get the first nth elements of the iterator
get_some(It,Num)->
    get_some(It,Num,[]).



get_some(_It,ZeroOrLess,Acc) when ZeroOrLess < 1 ->
    lists:reverse(Acc);
get_some(It,Num,Acc) ->
    case It() of
	false ->
	    lists:reverse(Acc);
	{Elem,	NewIt} ->
	    get_some(NewIt,Num-1,[Elem|Acc])
    end.



%% Tries to get one value from each iterator in the list. 
%% Fun is an evaluation function for the result
%% Returns an iterator
combine(ItList,EvFun)->
    Iterator = pile(ItList),
    iterator:create_iterator_fun(Iterator,EvFun).
    	  

    
%% Receives a list of iterators
%% Returns an iterator that can generate all possible permutations of
%% the elements taken from the iterators
%% e.g. [1,2] and [3,4] -> [1,3],[1,4],[2,3],[2,4]  
pile([])->
    iterator:create_iterator([]);
pile([It|ItList])->
    pile(ItList,It).


pile([],It)->
    It;
pile([It|Rest],AccumIt) ->
    Fun = fun (X) ->
		  %io:format("X: ~p~n",[X]),  
		  FunConcat = fun (ValueFromIt) when is_list(ValueFromIt)->
				      case X of
					  _ when is_list(X) ->
					      [ValueFromIt ++ X];
					  _ ->
					      
					      [[X|ValueFromIt]]
				      end; 
				  (ValueFromIt) ->
		%		      io:format("ValueFromIt: ~p~n",
		%				[ValueFromIt]),  

				      case X of
					  _ when is_list(X) ->
					      
					      [[ValueFromIt|X]];
					  _ ->
		%			      io:format("Returning: ~p~n",
		%					[[ValueFromIt,X]]),
					      [[X,ValueFromIt]]
				      end 
				  end,
		  iterator:create_iterator_fun(It,FunConcat)
		  end,
    pile(Rest,iterator:create_iterator_fun(AccumIt,Fun)). 


%% Returns true if Pred returns true for at least one of the 
%% elements generated by the iterator It
any(Pred,It)->
    case It() of
	false ->
	    %% No more elements
	    false;
	{Elem,NewIt} ->
	    try 
		case Pred(Elem) of
		    true ->
			true;
		    _ ->
			any(Pred,NewIt)
		end
	    catch
		_:_ ->
		    any(Pred,NewIt)
	    end
    end.
	    

