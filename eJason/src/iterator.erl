-module(iterator).

%-compile(export_all).
-export([create_iterator/1,create_iterator_fun/2,
	concat/2,get_all/1, first/1]).

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
