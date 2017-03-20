
-define(UNBOUND,not_bound). % 

-define(ISATOM, is_atom). %the "args" of a struct of arity 0

-define(ISLIST, is_list). %the "args" of a struct that is a list


-define(ISREF,functor_is_ref). % functor is a reference to a var which 
                                 % may or may not be matched yet

-define(STRONGNEG,strong_negation). 
		   %% functor is a reference to a var which 
                   %% is matched (it implies strong negation of that var).


-define(UNBOUNDLIST, unbound_list).
                   %% This kind of var is a list that remains to be matched
                   %% It works similarly to an unbound var, but it can only be
                   %% matched to a list.

-define(NOFUNCTOR,no_functor).



-define(EMPTYLISTVAR,  #var{id = '[]',
			    functor = {[],[{'[]'}]},
			    args = ?ISLIST,
			    annots = []}).

-define(ANYANNOTATION,ejason_any_annotation). %% Used to match two vars 
                                              %% ignoring the annotations

-record(var,
	{id = no_id, %% Name (given by eJason parser to atoms or given in Jason code)
	 functor = ?NOFUNCTOR,
	 args = ?UNBOUND, % If it contains vars, they are represented as {ID}
	 annots = []
	 % timestamp = erlang:timestamp(), %% Used to avoid collisions
	% bind = ?UNBOUNDVAR,
	 % is_ground = false,
	% unbound_vars=[] % only if it is not ground yet	 
	 }).

