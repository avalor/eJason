-module(test_goal).

-export([trigger/4,resolve/4]).

-include("include/parser.hrl").
-include("include/variables.hrl").
-include("include/macros.hrl").


trigger(BB,Bindings,Module,Query) ->
    ItTriggerBindings =
	iterator:create_iterator([Bindings]),
    Fun = fun (TriggerBindings) -> 
		  context(BB,TriggerBindings,Module,Query) end,
    iterator:create_iterator_fun(ItTriggerBindings,Fun).



context(BB,Bindings,Module,Query)->
    iterator:create_iterator([Bindings]).


resolve(BB,Bindings,Module,Query) ->
    ItRes =
	case  belief_base:query_bb(
		BB,Bindings,Module,Query) of
	    false -> 
		iterator:create_iterator([{?FAIL}]);
	    It when is_function(It) -> 
		It 
	end,
    
    Fun = fun (NewBindings) -> 
		  case NewBindings of
		      {?FAIL} -> false;
		      _ -> 
			  iterator:create_iterator([NewBindings]) 
		  end
	  end,
    iterator:create_iterator_fun(ItRes,Fun).    
