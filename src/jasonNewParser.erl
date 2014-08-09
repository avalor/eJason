%% Copyright (c) 2012-2014, Álvaro Fernández Díaz
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @author Álvaro Fernández Díaz
%% @copyright 2012-2013 Álvaro Fernández Díaz
%% @doc Provides all the functions necessary to transform eJason intermediate
%%      code to Erlang.


-module(jasonNewParser).

-export(
   [parse_internal_action/2, source_for_initial_belief/1, 
    source_for_initial_goal/1,
    make_belief/1, make_rule/1, make_event/2, make_plan/3,
    parse_agents/1, to_polish_notation/1]).

-include("include/parser.hrl").
-include("include/ejason.hrl").
-include("include/variables.hrl").
-include("include/macros.hrl").


%% -define(P(Name,Var),io:format("[~p]\n\t ~s: ~p\n",[?MODULE,Name,Var])).

-define(CONDITIONNUMBERERROR, 
	fun (_,_,_) ->
		io:format("[parser] CONDITION NUMBER USED TWICE~n"),
		exit(ejason_condition_parsed_twice) end).
		  


is_binary_operator(Operator)->
	lists:member(Operator,?BINARYOPERATORS).

is_atom_term(Term) ->
    lists:member(Term,?ATOMTERMS).

is_action(Param)->
    Param == action.


%% is_arithmetic_operator(Operator)->
%%     lists:member(Operator,?ARITHOPERATORS).


find_binding(Atom,Bindings) ->
    %% io:format("Look for: ~p~n",[Atom]),
    case orddict:find(Atom,Bindings) of
	{ok,Var} ->
	    Var;
	error ->
	    ?UNBOUND
    end.


%% Parse all the terms received from the scanner to create plans/rules/beliefs
%% It keeps track of the variables to create the bindings
%% e.g. an input term like {number,Line,1} is turned in to #var{id = 1, functor=1}
pre_parse([], Bindings,PStruct)->
 %% io:format("0~n~n"),
   NewPStruct = 
	case PStruct#parsing_struct.is_ordered of
	    true ->
		PStruct;
	    false ->
		NewAcc = lists:reverse(PStruct#parsing_struct.accum),
		PStruct#parsing_struct{
		  accum = NewAcc}
	end,
    %% io:format("SAlgo~n"),
    {Bindings,NewPStruct};
pre_parse([{var,Line,true}|Expr], Bindings,PStruct)->
%% io:format("2~n~n"),
    %% io:format("Parsing true\n"),
    pre_parse_atom([{atom,Line,true}|Expr],
				    Bindings,PStruct);
pre_parse([{var,Line,false}|Expr], Bindings,PStruct)->
%% io:format("3~n~n"),
   %%  io:format("Parsing false\n"),

    pre_parse_atom([{atom,Line,false}|Expr],
				    Bindings,PStruct);
pre_parse([{var,_Line,VarName}|Expr],
	  Bindings,
	  PStruct= #parsing_struct{info=Info,counter = Counter})->
    %% io:format("4~n~n"),

    %%    io:format("Var: ~p~nBindings: ~p~n",[VarName,Bindings]),
    {NewBindings,NewCode,_NewCounter} = 
	case Info of %% the input parameters (plans,rules)
                     %% SHALL be given different names
	   []->
%%TODO: erase and do the same in both cases. It should be dealt with 
%% in some other place
		case find_binding(VarName,Bindings) of
		    ?UNBOUND ->	
			NewVar = 
			    #var{id = VarName},
			{orddict:store(VarName,NewVar,Bindings),
			 NewVar,Counter};
		    Value ->
			{Bindings,Value,Counter}
		end;
	    params ->
		case find_binding(VarName,Bindings) of
		   
		    ?UNBOUND ->
			NewVar = 
			    #var{id = VarName},
			{orddict:store(VarName,NewVar,Bindings),
			 NewVar,Counter};
		    Value ->
			NewName =
			    list_to_atom(lists:flatten(
					   io_lib:format("~s",
							 [atom_to_list(
							    Value#var.id)]))),
			{Bindings,
			 Value#var{id = NewName},Counter}
		end
	end,
		
    Acc = PStruct#parsing_struct.accum,
    pre_parse(Expr, NewBindings,
	      PStruct#parsing_struct{
		accum = [NewCode|Acc]});


%%% Dealing with strong negation
pre_parse([{strongNeg,Term}|Expr],
	  Bindings,PStruct)->
 %% io:format("5~n~n"),
   Counter = PStruct#parsing_struct.counter,
    {TermBindings,NewPStruct} = 
	pre_parse([Term], Bindings,
		  #parsing_struct{
			    counter=Counter+1}),
    [NewTerm] = NewPStruct#parsing_struct.accum,
    NewVarID =
	list_to_atom(
	  "EJASONSTRONGNEGVAR"++integer_to_list(Counter)),
	  
    NewVar =
	#var{id = NewVarID,
	     functor = {NewTerm#var.id},
	     args = ?STRONGNEG,
	     annots = []},
    Counter1 = NewPStruct#parsing_struct.counter,
    Acc =  PStruct#parsing_struct.accum,
    
    NewBindings = variables:update(TermBindings,
				   [NewVar]),
    pre_parse(Expr, NewBindings,
			       PStruct#parsing_struct{
				 counter = Counter1,
				 accum = [NewVar|Acc]});

pre_parse([{critical_section, Formulas}|Expr],
	  Bindings,PStruct)->
    Counter = PStruct#parsing_struct.counter,
    {NewBindings,NewPStruct} = 
	pre_parse(Formulas, Bindings,
			  #parsing_struct{
			  counter=Counter}),    
    NewFormulas = NewPStruct#parsing_struct.accum,
    
    Counter1 = NewPStruct#parsing_struct.counter,
    Acc =  PStruct#parsing_struct.accum,
    pre_parse(Expr, NewBindings,
			       PStruct#parsing_struct{
				 counter = Counter1,
				 accum = [{critical_section,
					   NewFormulas}|Acc]});
  

%% TODO restrict values of pre_parse to those accepted. 
%% i.e. parse according to values, not the type of data.
pre_parse([{AddStruct,Term}|Expr],
	  Bindings,PStruct)->
%% io:format("6~n~n"),

    Counter = PStruct#parsing_struct.counter,
    {NewBindings,NewPStruct} = 
		pre_parse([Term], Bindings,
			  #parsing_struct{
				    counter=Counter}),
    
    [NewTerm] = NewPStruct#parsing_struct.accum,
%%    io:format("{AddStruct,NewTerm}: {~p,~p}~n",[AddStruct,NewTerm]),
    
    Counter1 = NewPStruct#parsing_struct.counter,
    Acc =  PStruct#parsing_struct.accum,
    pre_parse(Expr, NewBindings,
			       PStruct#parsing_struct{
				 counter = Counter1,
				 accum = [{AddStruct,
					   NewTerm}|Acc]});
pre_parse(List=[{action,_Atype,_Package,_Body}|_],Bindings,PStruct)->
 %% io:format("7~n~n"),
   pre_parse_action(List,Bindings,PStruct);
pre_parse(List=[{Head,_Elem1,_Elem2}|_],Bindings,PStruct)->
 %% io:format("8~n~n"),
   Binary = is_binary_operator(Head),
    Atom = is_atom_term(Head),
    Action = is_action(Head),
    IsList = Head == list,
    if
	
	Binary -> 
	    pre_parse_binary_operator(List,Bindings,PStruct);
	Atom -> 
	    pre_parse_atom(List,Bindings,PStruct);
	Action ->
	    pre_parse_action(List,Bindings,PStruct);
	
	IsList ->
	    pre_parse_list(List,Bindings,PStruct);	
	true ->
	    io:format("[JNP] ERROR: cannot parse ~p~n",[List]),
	    exit(parsing_error)
    end;
pre_parse([{formula,FunName,Terms,Label}|Expr],
			   Bindings,PStruct)->
%% io:format("9~n~n"),
    Counter1 = PStruct#parsing_struct.counter,
    {NewBinding1,NewPStruct1} =
	case FunName of
	    Atom when is_atom(Atom) ->
		pre_parse([{atom,0,Atom}], Bindings,
			  #parsing_struct{
					     is_ordered = true,
					     counter=Counter1});
	    {var,Line,VariableFunName} ->
	%	io:format("[JNParser] FunName is var: ~p\n\n",[FunName]),
	%	exit(error),
		pre_parse([{var,Line,VariableFunName}],Bindings,
			  #parsing_struct{
					is_ordered = true,
					counter=Counter1})
	end,

    Counter2 = NewPStruct1#parsing_struct.counter,
    %% io:format("Terms: ~p~n", [Terms]),
    {NewBinding2,NewPStruct2} =
	pre_parse(Terms, NewBinding1,
				   #parsing_struct{
					    info = params,
					    counter=Counter2}),
    
    %io:format("ParsedTerms: ~p~n", [NewBinding2]),

    Counter3 = NewPStruct2#parsing_struct.counter,

    {NewBinding3,NewPStruct3} =
	pre_parse(Label, NewBinding2,
				   #parsing_struct{
					    %is_ordered = true,
					    counter=Counter3}),
 

    Acc = PStruct#parsing_struct.accum,
    Counter4 = NewPStruct3#parsing_struct.counter,
    
    [NewAtom] = NewPStruct1#parsing_struct.accum,
    NewTerms = NewPStruct2#parsing_struct.accum,
    %% io:format("NewTerms: ~p~n", [NewTerms]),
    NewLabel = NewPStruct3#parsing_struct.accum,

    StripFun =
	fun (#var{id = ID}) -> {ID} end,
    
    Formula = 
	case {NewTerms,NewLabel}  of
	    {[],[]} ->
		%% A struct without args and annots is just a ref to the functor
		#var{id = list_to_atom("EJASONFORMULAVAR"++integer_to_list(Counter4)),
		     functor = StripFun(NewAtom), 
		     args =  ?ISREF, 
		     annots = []};
	    _ ->
	    
		#var{id = list_to_atom("EJASONFORMULAVAR"++integer_to_list(Counter4)),
		     functor = StripFun(NewAtom), 
		     args =  list_to_tuple(lists:map(StripFun,NewTerms)), 
		     annots = lists:map(StripFun,NewLabel)}
	end,
	%%make_predicate(NewAtom,NewTerms,NewLabel),
    
    pre_parse(Expr, orddict:store(
		      Formula#var.id,
		      Formula,
		      NewBinding3),
	      PStruct#parsing_struct{
		counter = Counter4+1,
		accum = [Formula|Acc]});

pre_parse(Other,_,_) ->
    io:format("[JNP] ERROR: cannot parse ~p~n",[Other]),
    exit(parsing_error).


%% Turns an atomic value (atom, string or number) into a variable
%% and adds it to the list of bindings
pre_parse_atom([{_,_Line,AtomValue}|Expr],
			   Bindings,PStruct)->
%% io:format("10~n~n"),
    Counter = PStruct#parsing_struct.counter,
    Info = PStruct#parsing_struct.info,
    %% io:format("AtomValue: ~p~n",[AtomValue]),
    {NewBindings,NewCode,NewCounter} =
	case Info of % TODO, erase case and handle in the same way
	    []->
		case find_binding(AtomValue,Bindings) of
		    ?UNBOUND-> %% atoms are stored as bound variables
			%% The key of a number is its corresponding atom
			Counter2 = Counter +1,
			NameForAtom =
			    case AtomValue of
				_ when is_atom(AtomValue)->
				    AtomValue;
				_ when 
				      is_list(AtomValue), 
				      length(AtomValue)=<10->
				    AtomValue;

				_ when is_list(AtomValue), 
				length(AtomValue)>=10 ->

				    "EJASONLONGTEXT"++integer_to_list(Counter);
				_ when is_number(AtomValue)->
				    AtomValue;
				_ ->
				  AtomValue
			    end,

			NewVar = 
			    #var{
			  id = NameForAtom,
			  functor = AtomValue,
			  args = ?ISATOM},
			{orddict:store(NewVar#var.id,NewVar,
				       Bindings), %new binding
			 NewVar,Counter2};
		    Value ->
			{Bindings,Value,Counter}
		end;
	    params ->

		StringNameForAtom =
		    if
			is_atom(AtomValue)->
			    atom_to_list(AtomValue);
			is_list(AtomValue), length(AtomValue)=<10->
			    AtomValue;
			is_list(AtomValue), length(AtomValue)>10 ->
			    "LONGTEXT";
			is_number(AtomValue)->
			    "NUMBER";
			true ->
			    ""
		    end,
		SVarName ="EJASONPARAMVAR"++integer_to_list(Counter)++
		    StringNameForAtom,
		%io:format("~n~nSVarName: ~p~n",[SVarName]),
		VarName = list_to_atom(SVarName),
		Counter2 = Counter +1,
		case find_binding(AtomValue,Bindings) of

		    ?UNBOUND->
			NewVar = 
			    #var{id = VarName,
                                 functor = 
				 {if 
				      %% is_float(AtomValue) ->
				      %% 	  list_to_atom(
				      %% 	    float_to_list(AtomValue));
				      %% is_integer(AtomValue)->
				      %% 	  list_to_atom(
				      %% 	    integer_to_list(AtomValue));
				      true ->
					  AtomValue 
				  end}, 
				 args = ?ISREF},

			NewVar2 = 
			    #var{
			  id = 
			  if 
		      %% is_float(AtomValue) ->
		      %% 	  list_to_atom(float_to_list(AtomValue));
		      %% is_integer(AtomValue)->
		      %% 	  list_to_atom(integer_to_list(AtomValue));
			      true ->
				  AtomValue
			  end,
			  functor = AtomValue,
			  args = ?ISATOM},
			
			%annots = []},
			% ADD the parameter and new atomic var to the bindings
			{orddict:store(VarName,NewVar,
				       orddict:store(NewVar2#var.id,
						     NewVar2,
						     Bindings)),
				       NewVar,Counter2};
		    Value when is_record(Value,var)-> %already valuated var

			NewVar = 
			    #var{id = VarName,
                                 functor = {AtomValue},%is a reference to AtomValue 
				 args = ?ISREF},


				   	%annots = []},
			%io:format("NewVar: ~p~n",[NewVar]),
		       %% ADD the parameter to the bindings
			{orddict:store(VarName,NewVar,Bindings),
			 NewVar,Counter2}


		    %io:format("SUSPICIOUS:~n  Value: ~p~n  "++
		%		  "AtomValue: ~p~n  Bindings: ~p~n",
		%		  [Value,AtomValue,Bindings])
		%	{Bindings,Value#var{id=VarName},
		%	 Counter2}
		end
	end,
    Acc = PStruct#parsing_struct.accum,
    pre_parse(Expr, NewBindings,
	      PStruct#parsing_struct{
		counter = NewCounter,
		accum = [NewCode|Acc]}).


pre_parse_list([List = {list,Header,Tail}|Expr],
			   Bindings,PStruct)->
%% io:format("11~n~n"),
    Counter = PStruct#parsing_struct.counter,
    Info = PStruct#parsing_struct.info,
   
     %% io:format("Parsing List: ~p~n",[List]),
    {NewBindings,NewPStruct} =
	if 
	    Header == []->
		{Bindings,
		 #parsing_struct{
		   counter=Counter+1}};
	    true ->
		pre_parse(Header, Bindings,
			  #parsing_struct{
				    counter=Counter+1})
	end,
%    io:format("NewStruct: ~p~n", [NewPStruct]),
 %   io:format("NewBindings: ~p~n", [NewBindings]),

    Counter2 = NewPStruct#parsing_struct.counter,
    
    NewCodeVars = NewPStruct#parsing_struct.accum,
    
    {NewBindings2,NewPStruct2} =

	if 
	    Tail == '[]' -> 
		{NewBindings,
		 #parsing_struct{
		   counter=Counter2}};
		 
	    true ->
		pre_parse([Tail], NewBindings,
			  #parsing_struct{
				    counter=Counter2})
	end,
    
	

  % io:format("NewStruct2: ~p~n", [NewPStruct2]),
    %% io:format("NewBindings2: ~p~n", [NewBindings2]),



    NewCodeRest = NewPStruct2#parsing_struct.accum,
    Counter3 = NewPStruct2#parsing_struct.counter,
    %io:format("NewCodeVars: ~p~n", [NewCodeVars]),
    %io:format("NewCodeRest: ~p~n", [NewCodeRest]),
    Acc = PStruct#parsing_struct.accum,




    Fun = fun(X, UseCounter)     -> 
		  case X of
		      %% #predicate{} ->
		      %% 	  {FunTopPredicate,FunNewVars}=
		      %% 	      predicate_to_vars("",
		      %% 				"EJASONPPLARGPARAM",
		      %% 				UseCounter,X),	  

		      %% 	  {{FunTopPredicate,FunNewVars},
		      %% 	   UseCounter+length(FunNewVars)+1};
		      #var{} ->
			  {{X,[]},UseCounter}
		  end
	  end,
          


    {ResultVariables, Counter4} =
	lists:mapfoldl(Fun, Counter3+1,
		       NewCodeVars),

%%    io:format("ResultVariables: ~p~n",[ResultVariables]),
    {TopVars,VarsInVariables} = 
	lists:unzip(ResultVariables),


    {ResultRest, Counter5} =
	lists:mapfoldl(Fun, Counter4,
		       NewCodeRest),

    %% io:format("ResultRest: ~p~n",[ResultRest]),

    {TopRest,VarsInRest} =
	lists:unzip(ResultRest),

    %% io:format("VarsInVariables: ~p~n",[VarsInVariables]),

    %% io:format("TopVars: ~p~n",[TopVars]),


    %% io:format("VarsInRest: ~p~n",[VarsInRest]),
    
    %% io:format("TopRest: ~p~n",[TopRest]),
    
    TotalVariables =
     	lists:foldl( fun (Var = #var{id = ID},Acum) ->
     			     orddict:store(
     			       ID,
     			       Var,
     			       Acum) end,
     		     NewBindings2,
		     lists:flatten(
		       lists:append([VarsInVariables,
				    VarsInRest,
				    TopVars,
				    TopRest]))),

%    io:format("TotalVariables: ~p~n",[TotalVariables]),
 
    EmptyListVar =
	?EMPTYLISTVAR,
	%% #var{ id = '[]',
	%%       functor = [],
	%%       args = ?ISATOM},



  %%   io:format("Tail for var: ~p~n~n~n~n~n",[Tail]), 
    %% io:format("NewCodeRest: ~p~n",[NewCodeRest]), 

    VarID = list_to_atom("EJASONLIST"++integer_to_list(Counter)),
    NewVar =
	#var{
      id = VarID,
%      functor = {[{X} ||#var{id =X} <-TopVars],
      functor = {[X || X = #var{} <-TopVars],
		 if
		     Tail == '[]' ->
			 [{'[]'}];
		    %%TODO: if the tail is another list, try to merge them
		     is_list(Tail) ->
		         [Y || Y = #var{} <-TopRest];
		     true -> %% A single Var
			 case NewCodeRest of
			     [RestVar =#var{args = ?UNBOUND}] ->
				 [RestVar#var{args = ?UNBOUNDLIST}];
			     _ ->
				 NewCodeRest
			 end
			 %io:format("SingleVar: ~p~n",[SingleVar]),
			 %[{SingleVar#var.id}]
		 end},
      args = ?ISLIST
     },
     %% io:format("Expr: ~p~n",[Expr]),

     %% io:format("Var for list: ~p~n",[NewVar]),
    pre_parse(Expr, orddict:store(VarID,
				  NewVar,
				  orddict:store(
				    EmptyListVar#var.id,
				    EmptyListVar,
				    TotalVariables)),
	      PStruct#parsing_struct{
		counter = Counter5,
		accum = [NewVar|Acc]}).


pre_parse_binary_operator([{Operator,Left,Right}|Expr],
			   Bindings,PStruct)->
     %% io:format("Binary Operation: ~p~n\n",[{Operator,Left,Right}]),
    
    Counter1 = PStruct#parsing_struct.counter,
    {NewBinding1,NewPStruct1} =
	pre_parse([Left], Bindings,
		  #parsing_struct{
			   %is_ordered = true,
			  counter=Counter1}),
    Counter2 = NewPStruct1#parsing_struct.counter,

    {NewBinding2,NewPStruct2} =

	case Operator of
	    log_not -> %% log_not does not have a right part, it is no_right
		{NewBinding1,
		 #parsing_struct{
		   counter=Counter2,
		   accum = [no_right_part]}
		 };
	    _ ->
		pre_parse([Right], NewBinding1,
			  #parsing_struct{
				     %%is_ordered = true,
				     counter=Counter2})
	end,
    
    Acc = PStruct#parsing_struct.accum,
    Counter3 = NewPStruct2#parsing_struct.counter,

    [NewLeft] = NewPStruct1#parsing_struct.accum,
    [NewRight] = NewPStruct2#parsing_struct.accum,

    NewOperation = #binary_operation{
      operator= Operator,
      left_part = NewLeft,
      right_part = NewRight},

    pre_parse(Expr, NewBinding2,
	      PStruct#parsing_struct{
		counter = Counter3,
		accum = [NewOperation|Acc]}).





%% Parses an action
pre_parse_action([{action,AType,Term}|Expr],
			   Bindings,PStruct)->
%% io:format("12~n~n"),
    %% This action is an operation e.g. A = B, A = 2+3..
    pre_parse_action([{action,AType,"",Term}|Expr],
			   Bindings,PStruct);
pre_parse_action([{action,AType,Package,Term}|Expr],
			   Bindings,PStruct)->
   %%  io:format("Parsing action: ~p~n",[Term]),
   %% io:format("Parsing Action with Package: ~p~n",[Package]),
%% io:format("13~n~n"),
    Counter = PStruct#parsing_struct.counter,
    {NewBindings,NewPStruct} = 
	pre_parse([Term],Bindings,
		  #parsing_struct{
			 counter=Counter}),

 
    [NewTerm] = NewPStruct#parsing_struct.accum,
    Counter1 = NewPStruct#parsing_struct.counter,
    
    %% io:format("NewTerm: ~p~n",
    %% 	      [NewTerm]),

   
    
    
    FinalBindings =
	case NewTerm of 
	    #binary_operation{} ->
		%% If the action is an operation, the bindings do not change
		NewBindings;
	    #var{}->
		%% If the action is an internal action, the bindings are 
		%% extended with a new struct var
		NewID =
		    list_to_atom("EJASONACTIONVAR"++integer_to_list(Counter1)),
  
		ActionVar = 
		    #var{id = NewID,
			 functor = {NewTerm#var.id},
			 args = ?ISREF,
			 annots = []},   
	 
		%% io:format("Predicate in Action: ~p~n",
		%% 	  [ActionVar]),
		orddict:store(
		  NewID,
		  ActionVar,
		  NewBindings)
	end,

    %% ImportVars =
    %% 	variables:vars_to_import(NewVar),
    
	  
	  
    %% {TopVar,RestVars} = 
    %% 	predicate_to_vars("","PredInAction", 100,
    %% 			  lists:nth(1,NewTerm)),

    %% ImportVars =
    %% 	lists:flatten(
    %% 	  lists:map(fun variables:vars_to_import/1,
    %% 		    [TopVar|RestVars])),


 	  
    %% io:format("NewVars from Action: ~p~n",
    %% 	      [ImportVars]),
	  
    %% FinalBindings =
    %% 	variables:update(NewBindings, ImportVars),

    NewAction = #action{type = AType,
			package = Package,
			body = NewTerm},
    %% io:format("NewAction: ~p~n",[NewAction]),

    Acc =  PStruct#parsing_struct.accum,
    pre_parse(Expr,FinalBindings ,
	      PStruct#parsing_struct{
		counter = Counter1+1,
		accum = [NewAction|Acc]}).




%% Generates a new belief to be added to the initial beliefs.
%% Each belief is a valuated var
%% TODO: consider adding beliefs not valuated, to allow belief predicates
make_belief(F={formula,_Atom,_Arguments,_Annotations})->
    {Bindings,PStruct} = pre_parse([F],[],#parsing_struct{}),
    %% io:format("Belief~n"),
    [Belief] = PStruct#parsing_struct.accum,
   % The bindings are used to valuate the belief, because beliefs are 
   % standalone structs.
    variables:valuate(Bindings,Belief).



%% Receives a formula and generates a rule
make_rule({F={formula,Atom,Arguments,Annotations},
	  eJasonRule,Conditions})->
 %   io:format("Formula for Rule: ~p~n",[F]),

    NewFormula = % all variables in the params are turned into formulas
	{formula,
	 Atom,
	 lists:map(fun({var,Line,VarName}) ->
			   {formula,{var,Line,VarName},[],[]};
		      (Other) ->
			   Other
		   end, Arguments),
	 Annotations},

  %  io:format("NewFormula for Rule: ~p~n",[NewFormula]),

    {Bindings,PStruct} =
	pre_parse([NewFormula],[],#parsing_struct{}),
    
    Counter = PStruct#parsing_struct.counter,

    {Bindings2,PStruct2} =
	pre_parse(Conditions,Bindings,#parsing_struct{
						counter = Counter}),


    Bindings3 = %%lists must be "cleaned"
	orddict:map( fun (_,X) ->
			     variables:clean_var(X) end,
		     Bindings2),
	  
    [Head] = PStruct#parsing_struct.accum,
    [Body] = PStruct2#parsing_struct.accum,
    %% io:format("RuleHead : ~p~nBindings: ~p~n",[Head,Bindings2]),

    #unparsed_rule{head = Head,
	  body = Body,
	  bindings = Bindings3}.

make_event(EventType, Body) ->
    {Bindings,PStruct} =
	pre_parse([Body], [],  #parsing_struct{}),

    [EBody] = PStruct#parsing_struct.accum,
     %% io:format("Making event: ~p~n",[Body]),
     %% io:format("Result: ~p~n",[EBody]),

    %%a = b,
    #event{type = EventType, 
	   body = Body}.%%Predicate#predicate{bindings=Bindings}}.
       


make_plan({TriggerType,
	   {var,Line,VarName}},
	  Context,Body)->
    %% The trigger is a free variable
    make_plan({TriggerType,
	       {formula,{var,Line,VarName},[],[]}},
	       Context,Body);
make_plan(Trigger= {TriggerType,
		    {formula,TriggerFunctor, TriggerArgs,TriggerAnnot}},
		    Context,Body)->
    %% io:format("Trigger: ~p~nContext: ~p~nBody: ~p~n",
    %% 	      [Trigger,Context,Body]),

    NewTrigger = % all variables in the trigger are turned into formulas
	{TriggerType,
	 {formula,TriggerFunctor,
	  lists:map(fun({var,Line,VarName}) ->
			    {formula,{var,Line,VarName},[],[]};
		       (Other) ->
			    Other
		    end, TriggerArgs),
	  TriggerAnnot}},	
 
 %   io:format("NewTrigger: ~p~n",[NewTrigger]),

    {Bindings,PStruct} =
	pre_parse([NewTrigger],[],#parsing_struct{}),

    Counter = PStruct#parsing_struct.counter,
    
    {Bindings2,PStruct2} =
	pre_parse([Context],Bindings,#parsing_struct{
					     is_ordered = true,
						counter = Counter}),

    Counter2 = PStruct2#parsing_struct.counter,
    %%io:format("Bindings2 (after context): ~p~n",[Bindings2]),
    

    {Bindings3,PStruct3} =
	pre_parse(Body,Bindings2, #parsing_struct{
					  counter = Counter2}),
    %TODO: maybe a make_event is more elegant
    [{TriggerType,Event}] = PStruct#parsing_struct.accum,
    [NewContext] = PStruct2#parsing_struct.accum,
    NewBody = PStruct3#parsing_struct.accum,

      %% io:format("Bindings3 (after body): ~p~n",[Bindings3]),
  
    Bindings4 = %%lists must be "cleaned"
	orddict:map( fun (_,X) ->
			     variables:clean_var(X) end,
		     Bindings3),
	  
    %% io:format("Bindings4 (after cleanup): ~p~n",[Bindings4]),

		  
		  
		      

    Plan = #unparsed_plan{trigger = 
		 #event{type = TriggerType,
			body = Event},
	  context = NewContext,
	  formulas = NewBody,
	  bindings = Bindings4},
  %  io:format("Resulting Plan: ~p~n",[Plan]),
    Plan.




%% Converts an external action into an internal action after finding a dot
parse_internal_action({no_package},{action, external_action,F}) ->
    %% io:format("Internal Action:: ~p~n",[F]),
    {action,internal_action,'.',F};
parse_internal_action({package,P},{action,external_action,F}) ->
    %% io:format("Package:packages then external: ~p~n",[P]),
    {action,internal_action,[P],F};
parse_internal_action({no_package},A={action,internal_action,_,_}) ->
    A;%%Duplicated dot
parse_internal_action({package,P},{action,internal_action,Packages,F}) ->
   %% io:format("Package:Packages then internal: ~p:~p~n",[P,Packages]),
    {action,internal_action,[P|Packages],F}.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PARSING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




generate_initial_beliefs(Beliefs)->
%    io:format("CALLED WITH PARAMS ~n~p~n",[Beliefs]),
       
    BeliefStrings = lists:map(fun jasonNewParser:source_for_initial_belief/1,
			      Beliefs),

    Header = "%% Function to includeInitial beliefs.\n\n"++
	"add_initial_beliefs(Agent = #agentRationale{})->\n",

    Body = string:join(BeliefStrings,",\n"), 

    io_lib:format(
      "~s~n"++
      "\treasoningCycle:applyChanges(Agent,[\n~s]).",
      [Header, Body]).
        

source_for_initial_belief(Belief= #var{id = ID}) ->
    %% {InitialBelief,OtherVars} = 
    %% 	predicate_to_vars("","EJASONINITIALBELIEF", 0,Belief),
    InitialBelief =
	Belief#var{id = 'EJASONINITIALBELIEF'},
    NewEvent = #event{type = ?ADDBELIEF, body = InitialBelief},
    io_lib:format("\t\t~p",[NewEvent]).
 

%% Generates the function that adds the initial rules to the
%% mental state of the agent.

generate_rules(Rules) ->
    generate_rules(Rules,{1,[]}).


generate_rules([],{_RuleNumber,Acc}) ->
    lists:flatten(
      io_lib:format(
	"%% Function that adds the initial rules\n" ++
	"add_rules(Agent = #agentRationale{belief_base = BB})->\n"++
	"  NewBB = belief_base:add_rules(BB,\n ~p),~n"++
	"  Agent#agentRationale{belief_base = NewBB}.\n\n",
	[Acc]));
generate_rules([{Name,Rules}|Rest],{RuleNumber, Acc}) ->
    %%io:format("Rules together: ~p~n",[Rules]),
    NumRules = length(Rules),
    SRulePosition = "_rule"++integer_to_list(RuleNumber)++"_",
    %%SRuleParams = source_many_vars("RuleParam",Arity),
    SRuleFunNames = string:join([io_lib:format(
				   "~s~spart~p",%%~s~sinit_~p
				   [atom_to_list(Name),SRulePosition,X])
				 || X <- lists:seq(1,NumRules)],",\n "), 
    
    %% SInitFun = 
    %% 	io_lib:format(
    %% 	  "\n\n~p(AgentInfo,CallBindings,Event)->\n"++
    %% 	  "ItFun = iterator:create_iterator([~s]) ,\n"++
    %% 	  "Fun = fun (X) -> apply(?MODULE,X,[AgentInfo,"++
    %% 	  "CallBindings,Event]) end,\n"++
    %% 	  "iterator:create_iterator_fun(ItFun,Fun).\n\n",
    %% 	  [Name,SRuleFunNames]),
    
   
    ParsedRules =
	[source_for_rule(Rule,SRulePosition,Counter) ||
	    {Rule,Counter} <- lists:zip(Rules,lists:seq(1,NumRules))],

    generate_rules(Rest,{RuleNumber+1, Acc++ParsedRules}).
			 %%orddict:store(Name,
			%%			    Rules,
			%%			    Acc)}).


%% Returns a record #parsed_rule
source_for_rule(Rule =
		#unparsed_rule{head = Head, body = Body, bindings = Bindings},
		SRulePosition,Counter) ->
    %% io:format("Rule to source: ~p~n",[Rule]),
    %% io:format("RuleBindings: ~p~n",[Bindings]),
    {Name} =
	(Head#var.functor),
    
    SFunName= atom_to_list(Name)++
	SRulePosition++ "part" ++ 
	integer_to_list(Counter),
    
    
    PStructConditions = source_for_conditions(SFunName,Body),
    %% io:format("[JNP] Conditions: ~p~n",[PStructConditions]),
    

    %% Function called if the trigger matches (body of a rule)
    %% It is always the one with number 1
    FirstConditionToCall = list_to_atom(
			     lists:flatten(io_lib:format("~s_~p",
							 [SFunName,	
							  1]))),
    
    
    Conditions = PStructConditions#parsing_struct.accum,
    #parsed_rule{name = Name,
		 trigger = Head,
		 context = Conditions,
		 bindings = Bindings,
		 first_condition = FirstConditionToCall}.
						    
    




    %% SourceForRule =source_for_inference_struct({rule,Bindings},Head,Body,
    %% 					    [FunName,FunName]),
    %% SourceForRule.

%% Derived from SourceForRule.
%% Header is a variable (atom or struct)
%% Body is a binary_operation record
%% TODO: design two versions for RULES & PLANS.
%%
%% Returns SourceCode, the functions that correspond
%% to the plan or rule
%% source_for_inference_struct(PLANORRULE,Header, Conditions,
%% 	     [HeaderFunName,ConditionsFunName]) -> 


%%     %% io:format("\n\nHeader: ~p~n",[Header]),
%%     %% Params = if
%%     %% 		 is_tuple(Header#var.args) ->
%%     %% 		     %% The header is a struct
%%     %% 		     tuple_to_list(Header#var.args)++Header#var.annots;
%%     %% 		 Header#var.args == ?ISREF ->
		     

%%     %% io:format("Params: ~p~n",[Params]),
   
%%     %% io:format("Conditions: ~p~n\n",[Conditions]),
    
 
%%     %% {_SArguments,NewParams} =
%%     %% 	PSArgs#parsing_struct.info,


%%     %% CleanFun =   %% some new variables are wrong
%%     %% 	fun (DirtyVar) ->
%%     %% 		variables:clean_var(DirtyVar) end,
    

%%     %% NewVariables = lists:map(CleanFun, UncleanVariables),
    
%%    %% io:format("SArguments: ~p~n",[_SArguments]),
%%     %% io:format("New Params: ~p~n",[NewParams]),
    



%%    %% io:format("New Variables: ~p~n",[NewVariables]),


%%     SFunName =  
%% 	case PLANORRULE of 
%% 	    plan ->
%% 		io_lib:format("~s(AgentInfo,Bindings,Event)",[HeaderFunName]);
%% 	    {rule,_} ->
%% 		io_lib:format("~s(AgentInfo,CallBindings,Event)",[HeaderFunName])
%% 	end,
   
   
       
%% %   io:format("Head: ~p~n",[Header]),
%% %    io:format("Body: ~p~n",[Body]),
%% %   io:format("SFunName: ~s->~n",[SFunName]),

%%     %% NumParams = length(NewParams),
    
    
%%     %io:format("SourceParamMatches: ~s~n",[SParamMatches]),
    
    
    
%%     %% PredsInAnnotations=
%%     %% 	source_for_annotations(Header#var.annots),
    
%%     %% SMatchAnnotations = io_lib:format("~p",[PredsInAnnotations]),
  
    
%% %%    io:format("MatchAnnotations: ~s~n",[SMatchAnnotations]),



%%    %% Defines which are the params in the header function
%%     SParamsGroup = "", 
%% 	%% io_lib:format("\tParams = [~s],\n",
%% 	%% 	      [source_many_vars("Param",NumParams)]), 
%% 	  %%  io:format("SParamsGroup: ~s~n",[SParamsGroup]),


%%     PStructConditions = source_for_conditions(ConditionsFunName,Conditions),
%% %    io:format("Info: ~p~n",[PStructBody#parsing_struct.info]),


%%     {FirstFun,_} = 
%% 	PStructConditions#parsing_struct.info,


%%     %% Function called if the trigger matches (context or body of a rule)
%%     FirstConditionToCall = lists:flatten(io_lib:format("~p_~p",
%% 						       [ConditionsFunName,
%% 							FirstFun])),
    
    
%%     STriggerMatches = 
%% 	case PLANORRULE of 
%% 	    plan -> 
%% 		source_trigger_matches(FirstConditionToCall,Header);
%% 	    %%NewParams);
%% 	    {rule,RuleBindings} ->
%% 		SBindings =  
%% 		    lists:flatten(
%% 		      io_lib:format(
%% 			"  CondBindings =  ~p,\n"++
%% 			"  Bindings =  variables:merge_bindings(CallBindings,"++
%% 			"CondBindings),\n",
%% 			[RuleBindings])),
		
		
%% 		SBindings ++ source_trigger_matches(FirstConditionToCall,Header)
%% 	end,



    
%%     SHeadRule = io_lib:format("~s->\n~s",
%% 			  [SFunName,STriggerMatches]),
    
%%     SConditions = PStructConditions#parsing_struct.accum,

%%   %  io:format("SRULE: ~s~n",[SHeadRule]),
%%  %%   io:format("SConditions: ~s~n",[SConditions]),
%%    "\n\n" ++ SHeadRule ++"\n\n" ++ string:join([SConditions],"\n\n").
    




%% Generates the proper set of condition tuples following
%% the corresponding conditions-tree
%% Condition tuple: {Name(Key), Module,Function,Args[,NextCondition]}
%%
%% Returns a parsing_struct where the "info" field indicates the
%% function in the root of the tree <- NO LONGER APPLIES... DELETE!**?
%% 
%% Returns a parsing_struct where the "accum" field is an orddict
%% of condition tuples
%%
%% Each condition specifies the next condition to be called.
%% This call will receive either a bindings or {?FAIL}
%% TODO: change {?FAIL} for "false" to include the case of empty iterators.
source_for_conditions(SFunName,Body)->
    Acc =  orddict:store(list_to_atom(SFunName++"_0"),
			 {conditions,return,[]},
			 orddict:new()),
    
    source_for_conditions(SFunName,Body,  
			  #parsing_struct{counter = 1,
					  info = 0, %% Return to 0
					  %% Nextfree is 2
					  accum = Acc}).


source_for_conditions(SFunName,{parenthesis,Condition},
		     PStruct) ->
    source_for_conditions(SFunName,
			  Condition,
			  PStruct); 


%% Each log_and generates a new condition. It executes the left
%% branch first. If successfully achieved, proceeds with the right one.
source_for_conditions(SFunName,#binary_operation{operator=log_and,
					       left_part = Left,
					       right_part = Right},
		      #parsing_struct{counter = Counter,
				      accum = Acc}) ->
    
    
    %% Parse the left branch
    #parsing_struct{
       accum = LeftAccum} = 
     	source_for_conditions(SFunName,Left,  
     			      #parsing_struct{counter = Counter+1}),

    %% Counter represents the numbers already used.
    %% Increase it with respect to the elements in the left branch
    RightCounter = orddict:size(LeftAccum) + Counter + 1,

    #parsing_struct{
       accum = RightAccum} = 
     	source_for_conditions(SFunName,Right,  
     			      #parsing_struct{counter = RightCounter}),

    
    NewAcc = 
	orddict:merge(?CONDITIONNUMBERERROR,
		      LeftAccum,
		      orddict:merge(?CONDITIONNUMBERERROR,
				    RightAccum,
				    orddict:store(
				      %% This condition is represented as 
				      %% element number "Counter"
				      list_to_atom(
					SFunName++"_"++
					    integer_to_list(Counter)),
				      {conditions,
				       logical_and,
				       %% The left branch starts with element
				       %% number "Counter+1"
				       [list_to_atom(
					  SFunName++"_"++
					      integer_to_list(Counter+1)),
					%% The right branch starts with element
					%% number "Rightcounter"
					list_to_atom(
					  SFunName++"_"++
					      integer_to_list(RightCounter))]},
				      Acc)
				   )
		     ),
   

    #parsing_struct{
       accum =  NewAcc};


 

%% When a logical or is found. An iterator with the two branches is created:
%% Fleft() and Fright().
%% Another iterator will call these branches in turn.
source_for_conditions(SFunName,#binary_operation{operator=log_or,
						 left_part = Left,
						 right_part = Right},
		      #parsing_struct{counter = Counter,
				      accum = Acc}) ->
    

    #parsing_struct{
       accum = LeftAccum} = 
	source_for_conditions(SFunName,Left,  
			      #parsing_struct{counter = Counter+1}),
    

    RightCounter = orddict:size(LeftAccum) + Counter + 1,

    %% Parse Struct  for the right branch of the logical and
    #parsing_struct{
       accum = RightAccum} = 
	source_for_conditions(SFunName,Right,
			      #parsing_struct{counter = RightCounter}),
    
    LeftRight = 
	orddict:merge( ?CONDITIONNUMBERERROR,
		       LeftAccum, RightAccum),
    
    NewAcc = 
	orddict:store(
	  list_to_atom(SFunName++"_"++integer_to_list(Counter)),
	  {conditions,
	   logical_or,
	   [list_to_atom(SFunName++"_"++integer_to_list(Counter+1)),
	    list_to_atom(SFunName++"_"++integer_to_list(RightCounter))]},
	  Acc),
    
    #parsing_struct{  accum = 	
			  orddict:merge(
			    ?CONDITIONNUMBERERROR,
			    LeftRight, NewAcc)};

source_for_conditions(SFunName, {strongNeg,
				 NegatedVar},
		      #parsing_struct{counter=Counter,
				      accum = Acc}) ->
  
    
    QueryVarID =
	list_to_atom(
	  lists:flatten(io_lib:format("EJASONQUERYFOR_~s_~p",
				      [SFunName,Counter]))),
    
    QueryVar = #var{
      id = QueryVarID,
      args = ?ISREF,
      functor = {NegatedVar#var.id}
     },
    
    


   NewAcc =
	orddict:store(
	  list_to_atom(SFunName++"_"++integer_to_list(Counter)),
	  {conditions,
	   strong_negation,
	   [QueryVar,
	    list_to_atom(SFunName++"_"++integer_to_list(0))]},
	  Acc),
    

    #parsing_struct{
       accum = NewAcc};

source_for_conditions(SFunName,#var{functor = {true}, args = ?ISREF},
		      #parsing_struct{counter = Counter,
				      accum = Acc}) ->
    NewAcc =
	orddict:store(
	  list_to_atom(SFunName++"_"++integer_to_list(Counter)),
	  {conditions,
	   true,
	   [list_to_atom(SFunName++"_"++integer_to_list(0))]},
	  Acc),
    
    
    #parsing_struct{accum = NewAcc};

source_for_conditions(SFunName,#var{functor = {false}, args = ?ISREF},
		      #parsing_struct{counter = Counter,
				      accum = Acc}) ->
    NewAcc =
	orddict:store(
	  list_to_atom(SFunName++"_"++integer_to_list(Counter)),
	  {conditions,
	   false,
	   [list_to_atom(SFunName++"_"++integer_to_list(0))]},
	  Acc),
    
    #parsing_struct{accum = NewAcc};



source_for_conditions(SFunName, Var= #var{},
		      #parsing_struct{counter=Counter,
				      accum = Acc}) ->
 
    
    QueryVarID =
	list_to_atom(
	  lists:flatten(io_lib:format("EJASONQUERYFOR_~s_~p",
				       [SFunName,Counter]))),
    QueryVar =
	#var{
      id = QueryVarID,
      args = ?ISREF,
      functor = {Var#var.id}
     },
    
    NewAcc =
	orddict:store(
	  list_to_atom(SFunName++"_"++integer_to_list(Counter)),
	  {conditions,
	   query_bb,
	   [QueryVar,
	    list_to_atom(SFunName++"_"++integer_to_list(0))]},
	  Acc),
    
    #parsing_struct{  accum = NewAcc};

source_for_conditions(SFunName,{log_not, Condition},
		      #parsing_struct{counter = Counter,
				      accum = Acc}) ->

   
    PSCondition =
	source_for_conditions(SFunName,
			      Condition,
			      #parsing_struct{
				counter = Counter+1}),
    
  
    ConditionAccum =
	PSCondition#parsing_struct.accum,

   NewAcc =
	%% Merging the condition tuples in the negated condition,
	%% the ones in Acc and adding the new log_not condition
	orddict:merge(
	  ?CONDITIONNUMBERERROR,
	  orddict:store(
	    list_to_atom(SFunName++"_"++integer_to_list(Counter)),
	    {conditions,
	     log_not,
	     [list_to_atom(SFunName++"_"++integer_to_list(Counter+1)),
	      list_to_atom(SFunName++"_"++integer_to_list(0))]},
	    Acc),
	  ConditionAccum),

    
	  
    #parsing_struct{accum = NewAcc};

source_for_conditions(SFunName,BO = #binary_operation{},
		      #parsing_struct{counter = Counter,
				      accum = Acc}) ->
    
    BinaryTuple = fun_for_binary_operation(BO),
    %%io:format("SConditionFun: ~s~n",[SConditionFun]),


    NewAcc = orddict:store(
	    list_to_atom(SFunName++"_"++integer_to_list(Counter)),
	    {conditions,
	     operation,
	     [BinaryTuple,
	      list_to_atom(SFunName++"_"++integer_to_list(0))]},
	    Acc),
    #parsing_struct{accum = NewAcc};

source_for_conditions(SFunName, Action ={action,
					internal_action,
					_Package,
					_Predicate},
		      #parsing_struct{counter = Counter,
				      accum = Acc}) ->
    
    {actions,internal_action,[Package,InternalAction]} =
	fun_for_internal_action(Action),
    
    NewAcc = orddict:store(
	       list_to_atom(SFunName++"_"++integer_to_list(Counter)),
	       {conditions,
		internal_action,
		[Package, InternalAction,
		 list_to_atom(SFunName++"_"++integer_to_list(0))]},
	       Acc),
    
    #parsing_struct{accum = NewAcc};


source_for_conditions(SFunName,Expression,_) ->
    io:format("Problems found parsing logical conditions for the plan or rule"++
	      "~s. Please, try adding some "++
	      "parenthesis to simplify parsing.~nExpression: ~p~n",
	      [SFunName,Expression]),
    exit({error,{jasonNewParser,source_for_conditions,
		 [SFunName,Expression]}}).
			     




%% Checks which will be the next function that must be called
%% It is increased once per "logical and" in the left/right branches
find_next(#binary_operation{operator=log_and,
			     left_part = Left},
	  Right,
	  {_Counter,NextFree}) ->
    %% This "logical and" binary operation will represent function #NextFree
    find_next(Left,Right,{NextFree,NextFree+1});
find_next(_,Right, {Counter,NextFree})->
    find_next(Right,{Counter,NextFree+1}).


find_next(#binary_operation{operator=log_and,
			     left_part = Left,
			    right_part = Right},
	  {_Counter,NextFree}) ->
    find_next(Left,Right,{NextFree+1,NextFree+2});
find_next(_,{Counter,_NextFree}) ->
    Counter.



%% Returns the function for a binary operation (used in plan contexts and
%% rules)
%%
%% Returns a tuple: 
%%                   {operations,log_not,{M,F,A}} 
%%                   {operations,operation,A}
fun_for_binary_operation(#binary_operation{operator = Operator,
					  left_part = Left,
					  right_part = Right}) ->
    case Operator of
	log_not->
	    {operations,log_not, fun_for_not(Left)};
	_ ->
	    {operations,operation,  [Operator,
			  branch_of_binary(Left),
			  branch_of_binary(Right)]}
    end.

%% Parses the branch of a binary operation.
%% Replaces the variables by a reference to them.

branch_of_binary(#var{id = VarID}) ->
    {VarID};
branch_of_binary({parenthesis,
		 BO}) ->
    {parenthesis,
     branch_of_binary(BO)};  

branch_of_binary(BO= #binary_operation{left_part = Left,
					right_part =Right}) ->
    BO#binary_operation{
      left_part = branch_of_binary(Left),
      right_part = case Right of 
		       no_right_part->
			   no_right_part;
		       _ ->
			   branch_of_binary(Right)
		   end
     }.



%% Not operations require the reception of a value as parameter.
%% A string to add as right part of a function operation is created here.
%%
%% If it is a binary operation, it suffices to resolve it
fun_for_not(BO = #binary_operation{}) ->
		 fun_for_binary_operation(BO);
%% If it is a variable struct, a query to the bb must be done
fun_for_not(#var{id = ID}) ->
    {belief_base,query_bb,[{ID}]};
%% If it is an internal action, the internal action must be invoked
fun_for_not(Action = {action, internal_action,_,_}) ->
    fun_for_internal_action(Action).


%% Prepares an internal action to be executed ({M,F,A} tuple)
%% Returns {actions,,internal_action,[NewPackage,Predicate]}
fun_for_internal_action(
			{action,
			 internal_action,
			 Package,
			 Predicate}) ->
  %%  io:format("MyPackage: ~p~n",[Package]),
    NewPackage =
	case Package of
	    '.' ->
		'.';
	    _ when is_list(Package) ->
		[Atom || {atom,_,Atom} <- Package]
	end,
    
    {actions,internal_action,[NewPackage,Predicate]}.





%% %% Function that represents an internal action in conditions 
%% %% (plan context or rule)
%% fun_for_internal_action(Action =
%% 			{action,
%% 			 internal_action,
%% 			 Package,
%% 			 Predicate}) ->
%%   %%  io:format("MyPackage: ~p~n",[Package]),
%%     NewPackage =
%% 	case Package of
%% 	    '.' ->
%% 		'.';
%% 	    _ when is_list(Package) ->
%% 		[Atom || {atom,_,Atom} <- Package]
%% 	end,
    
%%     {actions,internal_action,NewPackage,Predicate}).



generate_initial_goals(Goals)->
    %io:format("InitGoals: ~p~n",[Goals]),
    GoalStrings = lists:map(
		    fun (Goal) ->
			    jasonNewParser:source_for_initial_goal(Goal)
		    end,
		    Goals),

    Header = "%% Function to include all initial goals.\n\n"++
	"add_initial_goals(Agent = #agentRationale{})->\n",

    Body = string:join(GoalStrings,",\n"), 

    Res = io_lib:format(
	    "~s~n"++
	    "\treasoningCycle:applyChanges(Agent,[\n~s]).",
	    [Header, Body]),
%    io:format("InitialGoals: ~s~n",[Res]),
    Res.


source_for_initial_goal(Event = #event{body = Body}) ->
   
    {Bindings,PSBody} =pre_parse([Body],
			 [],
		      #parsing_struct{}),
    [VarBody]= PSBody#parsing_struct.accum,
    NewBody =
	variables:valuate(Bindings,VarBody),
	
     %% io:format("Body of initial goal: ~p~n",
     %% 	      [NewBody]),
    
    %% io:format("JNP Goal Body: ~p~nTurned into: ~p~n",
    %% 	      [Body, {Name,Args,Annot}]),
    
%    Var

    io_lib:format("\t\t~p",
		  [Event#event{
		     body = NewBody,
		     corrected_bindings = []}]).



generatePlans(Plans) ->
  %  io:format("Plans: ~p~n",[Plans]),
    PStructPlans = source_for_plans(Plans,#parsing_struct{counter = 1,
							accum = "",
							plan_base =
							orddict:new()}),
    SPlans = PStructPlans#parsing_struct.accum,
    SPlanRecords = PStructPlans#parsing_struct.plan_base,
%%      io:format("SourceForPlans PLANS: ~s~n",[SPlanRecords]),
    
    SAddPlans =
	io_lib:format("\nadd_plans(Agent = #agentRationale{})->\n"++
		      "\tAgent#agentRationale{\nplans = \n~s\n}.\n\n",
		      %% "\t#plan{trigger=fun test_goal:trigger/6,~n"++
		      %% "\tcontext=fun test_goal:context/6,\n"++
		      %% "\tbody=[fun test_goal:formula/1],\n"++
		      %% "\tbindings= []}\n"++
		      %%"\t]}.\n\n",
		      [SPlanRecords]),
   

%    io:format("SPlans: ~s~n",[SPlans]),
    SAddPlans ++ SPlans.



%% Parses the plan set and generates:
%% The set of plan records (PStruct.plan_base)
%%%%%%%%% b) The erlang functions for every plan (PStruct.accum)

source_for_plans([],PStruct) ->
    %% All plans parsed, generate source

    PlanBase = PStruct#parsing_struct.plan_base,

    

    %% io:format("PlanBase: ~s~n",[PlanBase]),
    Fun =
	fun ({Key,SRecordList})->
		%io:format("Key: ~p~n~s~n",[Key,SRecordList]),
		io_lib:format("{~p, [~s]}",
			      [Key,string:join(SRecordList,",")]) end,
    SPlanBase = lists:map(Fun, PlanBase),
    
    SOrddict = io_lib:format(
		 "[~s]", [string:join(SPlanBase,",")]),
	
    PStruct#parsing_struct{plan_base = 
			   SOrddict};

source_for_plans([#unparsed_plan{trigger = #event{type=Type,body=TBody}, 
				context = Context,
				formulas = Formulas, bindings = Bindings}
		|Plans],#parsing_struct{counter = Counter,
					accum = Acc,
					plan_base = PlanBase})->
     %% io:format("PlanAhora: ~p~n",[PlanAhora]),
     %% io:format("TBody: ~p~n",[TBody]),
    %% io:format("Context: ~p~n",[Context]),

    %% io:format("BindingsBefore parsing plan body: ~p~n",
    %% 	      [Bindings]),

    %% TypePlanVar = #var{id = 'EjasonEventType',
    %%  		       functor = Type,
    %%  		       args = ?ISATOM} ,

    {TriggerBodyName} = 
	(TBody#var.functor),

    %% PlanNameVar = #var{id = 'EjasonPlanName',
    %% 		       functor = TriggerBodyName,
    %% 		       args = ?ISATOM},
    
    
    %% TBodyParams = TBody#var.args,

    String = 
    	atom_to_list(TriggerBodyName),
    
	

    SFunName = 
    	case string:str(String,"Ejason_")of
    	    1 ->
    		%% The trigger is an unbound variable!
    		"free_var_"++string:substr(String,8)++
    		    "_"++ integer_to_list(Counter);
    	    _ ->
    		%% The trigger is not an unbound variable
    		String++"_"++
    		    integer_to_list(Counter)
    	end,

    %% io:format("SFunName: ~s~n",[SFunName]),


    %% HeaderName = list_to_atom(SFunName++"_trigger"),
    ContextName = SFunName++"_context",
    
    %% STriggerContext=
    %% 	source_for_inference_struct(plan,
    %% 				 TBody,%%#var{args = TriggerParams},
    %% 				 Context,
    %% 				 [HeaderName,BodyName]),
    %% io:format("JNO NewParamVariables: ~p~n", [NewParamVariables]), 
  
    PStructConditions = source_for_conditions(ContextName,Context),
				

%%   io:format("Info: ~p~n",[PStructBody#parsing_struct.info]),


    %% {FirstFun,_} = 
    %% 	PStructConditions#parsing_struct.info,
   %% io:format("FirstFun: ~p~n",[FirstFun]),


    %% Function called if the trigger matches (plan context or body of a rule)
    FirstConditionToCall = list_to_atom(
			     lists:flatten(io_lib:format("~s_~p",
							 [ContextName,
							  1]))),


     PSFormulas = 
     	source_for_plan_formulas(Formulas,
     			      #parsing_struct{accum = ""}),

    SFormulas = PSFormulas#parsing_struct.accum,
     %%	io_lib:format("~s_last_formula(Params,Bindings)->\n"++
     %%		     "\tutils:get_params("),

    ContextOrddict =
	PStructConditions#parsing_struct.accum,

    
    NewBindings = 
	variables:update(Bindings,
			 [?EMPTYLISTVAR]),
    
    % Bindings and newParamVariables are merged
    SPlanRecord = generate_plan_record(
		    TBody,
		    FirstConditionToCall,
		    ContextOrddict,
		    SFormulas,
		    NewBindings),
    
		 %  orddict:merge(fun (K,V1,_) -> V1 end,
		%		 Bindings,NewParamVariables)),
    


    %%io:format("~n~nTriggerContext: ~s~n",[STriggerContext]),
    source_for_plans(Plans,
		   #parsing_struct{counter = Counter+1,
				   accum = "",%%Acc++STriggerContext,
				   plan_base = orddict:append(
						 Type,SPlanRecord,
						 PlanBase)}).


%% %% Returns the number of formulas in a plan body
%% count_formulas(Formulas) ->
%%     count_formulas(Formulas,0).

%% count_formulas([],Accum)->
%%     Accum;
%% count_formulas([{critical_section,CriticalSection}|Rest],Accum) ->
%%     count_formulas(Rest,Accum+count_formulas(CriticalSection));
%% count_formulas([_Formula|Rest],Accum) ->
%%     count_formulas(Rest,Accum+1).

  
generate_plan_record(TBody, FirstContextFun, Context, SFormulas,Bindings)->
 %   io:format("Plan Record Bindings: ~p~n",[Bindings]),
   
    %% io:format("FirstContextFun: ~p~n",[FirstContextFun]),
      
    io_lib:format(
      "\n#plan{trigger= {~p,~p},~n"++
      "  context= ~p,\n"++
      "  body=[~s],\n"++
      "  bindings= ~p}",
      [TBody,FirstContextFun,Context,
       string:join(SFormulas,",\n\t"),Bindings]).



%% %% Produces the source code for the "body" of a plan record
%% plan_record_formulas(SPlanName,Formulas) ->
%%     plan_record_formulas(SPlanName,1,[],Formulas).



%% plan_record_formulas(_SPlanName,_Num,Accum,[]) ->
%%     string:join(lists:reverse(Accum), ",\n");
%% plan_record_formulas(SPlanName,Num, Accum,
%% 		     [{critical_section,CriticalFormulas}|Rest]) ->
%%     %% Critical section. 
    
%%     SCriticalFormulas = 	
%% 	string:join(
%% 	  [io_lib:format("   {?NAME,~s_body_formula_~p}",
%% 			 [SPlanName, X])|| 
%% 	      X <- lists:seq(Num,Num+length(CriticalFormulas))],
%% 	  ",\n"),


%%     SCriticalSection =
%% 	io_lib:format("{critical_section,[\n~s]}",
%% 			 [SCriticalFormulas]),
%%     plan_record_formulas(SPlanName,Num+length(CriticalFormulas),
%% 			 [SCriticalSection|Accum],Rest);
%% plan_record_formulas(SPlanName,Num,Accum,[_|Rest]) ->
%%     %% Normal formula
%%     SFormula = io_lib:format("{?NAME,~s_body_formula_~p}",
%% 			     [SPlanName, Num]),
    
%%     plan_record_formulas(SPlanName,Num+1,
%% 			 [SFormula|Accum],Rest).
	
    



%% Generates the source for the formulas in the body of a plan
%%% Each plan formula is a tuple {M,F,A}
%% Critical sections are tuples {critical_section,[{M,F,A},...,{M',F',A'}]}
source_for_plan_formulas([],PStruct)->
    PStruct;

source_for_plan_formulas([{critical_section,CriticalFormulas}|
		       Formulas], PS=#parsing_struct{accum = Accum})->
    CriticalPS =
	source_for_plan_formulas(CriticalFormulas,
			      #parsing_struct{  accum = []}),	     

    AccumCritical = string:join(
		      CriticalPS#parsing_struct.accum, ",\n"),

    source_for_plan_formulas(Formulas,
			  PS#parsing_struct{
			    accum = Accum++["{critical_section,["++
					    AccumCritical++"]}"]}); 
source_for_plan_formulas([ #action{type = Type,package=Package,body = Body}|
			Formulas], 
		      #parsing_struct{
				      accum = Acc,
				      info = _SFunName}) ->
    %% io:format("Plan-Body Action: ~p~n",[Ac]),
    
    DSBody = source_for_predicate(Body), %%dirty_sbody
    
    %%  io:format("Result SourceForPredicate for a formula: ~n\t\t~p~n",
    %%        [DSBody]),

    SBody = case DSBody of %% cleaning sbody
		#binary_operation{left_part = DBodyLeft,
				  right_part = DBodyRight} ->
		    DSBody#binary_operation{
		      left_part =
		      variables:clean_var(DBodyLeft),
		      right_part =variables:clean_var(DBodyRight)};
		_ ->
		    variables:clean_var(DSBody)
	    end,
    
   %% The body formulas require three arguments ->
   %%   {M,F,A}
    {Module,Function,Args}  =
	case Type of
	    'fun' ->
		%io:format("Type: ~p~n",[Type]),
		
		%%This operation takes the bindings as param
	

		FunArgs =
		    [Body#binary_operation.operator,
		     SBody#binary_operation.left_part,
		     SBody#binary_operation.right_part],
	
%		io:format("Body: ~p~n",[Body]),
		      {operations,operation,FunArgs};
	    external_action ->
		{actions,external_action,
		 [SBody]};
	    internal_action ->
		{actions,internal_action, 
		 [Package,SBody]}; 
	    
	    ?ADDACHGOAL ->
		{utils,add_achievement_goal,
		 [SBody]};
	    
	    ?ADDBELIEF ->
		{utils,add_belief, [SBody]};
	    
	    ?REMOVEBELIEF ->
		{utils,remove_belief,
		 [SBody]};

	    ?REMOVEADDBELIEF ->
		{utils,remove_add_belief,
		 [SBody]};
	    ?ADDINTENTIONGOAL ->
		{utils,new_intention_goal,
		 [SBody]};

	    ?ADDTESTGOAL ->
		{actions,test_goal,
	    %% Needs AgentInfo to call query_bb
		 [SBody]};

	    ?ADDNOWAITTESTGOAL ->
		{actions,no_wait_test_goal,
	    %% Needs AgentInfo to call query_bb
		 [SBody]};
	    ?FAILEDACHGOAL ->
		{utils,failed_achievement_goal,
		 [SBody]};

	    ?FAILEDTESTGOAL ->
		{utils,failed_test_goal,
		 [SBody]};
	    _ ->
		io:format("Unknown type of formula ~p  in body: ~p~n",
			  [Type,SBody]),
		exit(unhandled_in_parse),
		{Type,
		 "?MODULE",
		 io_lib:format("~p",[SBody])}     
		 %io_lib:format("variables:valuate(Bindings,~p)",[SBody])}
	end,


    %%Added to diminish warning when compiling generated code
    %% SAgentInfo =
    %% 	case Type of
    %% 	    internal_action ->
    %% 		"AgentInfo";
    %% 	    external_action ->
    %% 		"AgentInfo";
    %% 	    ?ADDTESTGOAL->
    %% 		"AgentInfo";
    %% 	    ?ADDNOWAITTESTGOAL->
    %% 		"AgentInfo";
    %% 	    _ ->
    %% 		"_AgentInfo"
    %% 	end,

    %% SFormula = 

    %% 	io_lib:format("~s_body_formula_~p(~s,Bindings)->\n"++
    %% 		      ""++
		      
    %% 		      "~s(~s).\n\n",
    %% 		      [SFunName,Counter,SAgentInfo]++
    %% 		      [Call,lists:flatten(
    %% 			      string:join(Args,","))]),


    SFormula = io_lib:format("~p",[{Module,Function,Args}]),

%    io:format("SFormula: ~s~n",[SFormula]),

    source_for_plan_formulas(Formulas,
			  #parsing_struct{
			    accum = Acc++[SFormula]
			    %% lists:append([VarsInBody,
			    %% 	      VarsAcc])
			   }).




%% Also deals with the action binary operations in a plan body
%% Returns a new variable
source_for_predicate(Var= #var{id = ID}) ->  
 
    %% io:format("JNP-> source_for_predicate: ~p~n",
    %% 	      [P]),
     
    NewVarID =
	list_to_atom("EJASONBODYFORMULA" ++
		     variables:make_timestamp_string()),
    	
    Res = #var{
      id = NewVarID,
      functor = {ID},
      args = ?ISREF},
%%    io:format("Source: ~p~n",[Res]),
    Res;
source_for_predicate({parenthesis,Pred}) ->
    #binary_operation{operator = parenthesis,
		      left_part = source_for_predicate(Pred)};
source_for_predicate({opposite,Pred}) ->
    #binary_operation{operator = opposite,
		      left_part = source_for_predicate(Pred)};
source_for_predicate(BO = #binary_operation{
					 left_part = Left,
					 right_part = Right}) ->
    BO#binary_operation{
      left_part = source_for_term(Left),
      right_part = source_for_term(Right)}.






%% source_for_terms(Vars)->
%%     lists:map(fun jasonNewParser:source_for_term/1, Vars).



%% TODO find where the binary operators are created deal with
%% parenthesis and opposite there.
source_for_term({parenthesis,Pred}) ->
    #binary_operation{operator = parenthesis,
		      left_part = source_for_predicate(Pred)};
source_for_term({opposite,Pred}) ->
    #binary_operation{operator = opposite,
		      left_part = source_for_predicate(Pred)};
source_for_term(BO) when is_record(BO,binary_operation)->
    source_for_predicate(BO);
source_for_term(Var = #var{}) ->
    {Var#var.id};
%% source_for_term(#var{id=ID}) -> % Bind is atom or integer
%%     ID;
%source_for_term(Atom) when is_atom(Atom)->
%    Atom;
source_for_term([]) ->
    [].




%% Generates the calls at the beginning of a plan trigger
%% that check if the event "pulls the triggers"
source_trigger_matches(FirstCondition,Event) ->
    %% io:format("Source_trigger_matches for event: ~p~n",[Event]),

     io_lib:format(
       "   case variables:match_vars(Bindings,~p,Event) of\n"++
       "     false -> false; %% trigger does not match\n"++
       "     ItTriggerBindings -> \n"++
       "       Fun = fun (TriggerBindings) -> ~s(AgentInfo,"++
       "TriggerBindings) end,\n"++
       "       iterator:create_iterator_fun(ItTriggerBindings,Fun)\n"++
       "    end.\n\n",
       [Event,FirstCondition]
      ).



%%% Turns an arithmetic expression into prefix (polish) notation.

%% Vars and number are left unchanged
to_polish_notation(Var ={var,_,_})->
    Var;
to_polish_notation(Number={number,_,_}) ->
    Number;
to_polish_notation(Expression) ->
    InfixList =
	make_proper_infix(Expression),
    PrefixList = infix_to_polish_algorithm(InfixList),
    process_prefix(PrefixList).


    %% 1.- Scan the input expression from right to left.
    %% 2.- If the scanned character is an operand,add to the prefix list.
    %% 3.- If the scanned character is an operator and the stack is empty,
    %%     push the character on the stack.
    %% 4.- If the scanned character is ‘)’ , then push it on the stack.
    %% 5.- If the scanned character is ‘(‘ then pop the characters from 
    %%        the stack
    %%        till matching ‘)’ is found.
    %% 6.- If the scanned character is an operator and the stack is not 
    %%        empty and 
    %%    the priority of the scanned character is more than that of the 
    %%    operator on top of the  stack then push the character on the stack.
    %% 7.- If the scanned character is an operator and the stack is not 
    %%     empty and
    %%    the priority of the scanned character is less than that of the 
    %%    operator on top of the  stack then pop the contents of the stack 
    %%    into the list till this character can be pushed on to the stack.
    %% 8.- Prefix the elements in the stack (top to bottom) to the list


  %%First, turn the parsed expression into a proper infix notation list 
make_proper_infix(Var = {var,_,_})->
    [Var];
make_proper_infix(Num = {number,_,_}) ->
    [Num];
make_proper_infix({parenthesis,Values}) ->
    [open_parenthesis|make_proper_infix(Values)] ++[close_parenthesis];
make_proper_infix({opposite,Values}) ->
    %% -Expression -> (-1 * Expression) 
    [open_parenthesis,{number,0,-1},arith_mult|make_proper_infix(Values)] ++
	[close_parenthesis];
make_proper_infix({Operator,Left,Right}) ->
  lists:flatten(make_proper_infix(Left)++[Operator]++make_proper_infix(Right)).


%% Return a list equivalent to the prefix notation.
%% That list must be processed before it can be fed to the parser again
infix_to_polish_algorithm(InfixList) ->
    %% io:format("Infix to Polish: ~p~n",[InfixList]),
    infix_to_polish_algorithm(lists:reverse(InfixList),
			      [],[]).


infix_to_polish_algorithm([],ResultList,Stack)->
    %% Case 8
    lists:reverse(Stack)++ResultList;
infix_to_polish_algorithm([Operand ={OperandType,_,_}|Rest],
			  ResultList,Stack) when OperandType == var;
						 OperandType == number->
    %% Case 2
    
    infix_to_polish_algorithm(Rest,
			      [Operand|ResultList],
			      Stack);

infix_to_polish_algorithm([Operator|Rest],
			  ResultList,[])  ->
    %% Case 3
     infix_to_polish_algorithm(Rest,
			       ResultList,
			      [Operator]);
infix_to_polish_algorithm([close_parenthesis|Rest],
			 ResultList,Stack) ->
    %% io:format("NewStack = ~p~n",
    %% 	      [[close_parenthesis|Stack]]),
    %% Case 4
     infix_to_polish_algorithm(Rest,
			       ResultList,
			       [close_parenthesis|Stack]);
infix_to_polish_algorithm([open_parenthesis|Rest],
			  ResultList,Stack) ->
    %% Case 5
    %% io:format("Looking for close_parenthesis in: ~p~n",[Stack]),
    {InsideParenthesis,[close_parenthesis|OutsideParenthesis]} =
	lists:splitwith(fun(close_parenthesis) ->
				false;
			   (_) ->
				true
			end,
			Stack),
    
     infix_to_polish_algorithm(Rest,
			       lists:reverse(InsideParenthesis) ++ResultList,
			       OutsideParenthesis);
infix_to_polish_algorithm([Operator|Rest],
			  ResultList,Stack)  ->

    {HigherPrecedence, LowerPrecedence} =
	lists:splitwith(fun (OperatorInStack)->
				lower_precedence_order(Operator,
							OperatorInStack)
			end,
			Stack),

    
    %% io:format("Operator: ~p~nHigherPrecedenceOrder: ~p~n",
    %% 	      [Operator,HigherPrecedence]),

    %%Cases 6 and 7
    
    infix_to_polish_algorithm(Rest,
			      lists:reverse(HigherPrecedence) ++ResultList,
			      [Operator|LowerPrecedence]).


%% Returns true if operator1 has lower precedence order than operator2
%% The precedence order is: (div,mod,**,*,/) > (+,-)

lower_precedence_order(OperatorType,OperatorType) ->
    false; %% The operator in the stack has lower precedence
lower_precedence_order(OperatorType1,OperatorType2) ->
    
    OrderFun =
	fun (Operator)->
		case lists:member(Operator,
				  [arith_mult,arith_power,
				   arith_slash, arith_div, arith_mod]) of
		    true ->
			high;
		    false ->
			low %% Either +, -, or close_parenthesis
		end
	end,
    
    OrderOperator1 =
	OrderFun(OperatorType1),
     OrderOperator2 =
	OrderFun(OperatorType2),
    

    case {OrderOperator1,OrderOperator2} of
	{low, high}->
	    true;
	_ ->
	    false
    end.

    

%% Turns a list in prefix notation to the proper expression:


  %% 1.- For each element (right to left)
  %% 2.- If the element is an operand, put it on top of the stack
  %% 3.- If the element is an operator, apply it to the two elements on top
  %%     of the stack and add the result to the top of the stack

process_prefix(PrefixList)->
    process_prefix(lists:reverse(PrefixList),
		   []).

process_prefix([],
	       [Expression]) ->
    Expression;
process_prefix([],Stack) ->
    io:format("Incorrect result stack when processing prefix term:\n"++
	      "~p should be a singleton~n",
	      [Stack]),
    exit(parsing_arithmetic_expression_error);
process_prefix([Operand = {_,_,_}|Rest], Stack) ->
    %% Case 2
    process_prefix(Rest,[Operand|Stack]);
process_prefix([Operator|Rest],[Operand1,Operand2|Stack]) 
  when  is_atom(Operator)->
    %% Case 3
    NewOperand = 
	{Operator,Operand1,Operand2},
    process_prefix(Rest,[NewOperand|Stack]).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% ERLANG AGENTS
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_agent_start(Name,EnvironmentName)->
    Module = io_lib:format("-module(~p).~n~n",[Name]),
    Export = "-export([start/1]).\n\n",
    Import = "-include(\"macros.hrl\").\n"++
	"-include(\"ejason.hrl\").\n"++
	"-include(\"variables.hrl\").\n\n",
    NameMacro = io_lib:format("-define(NAME,~p).~n",[Name]),
    InternalMacro = io_lib:format("-define(INTERNAL,~p).~n",[Name]),
    EnvironmentMacro =io_lib:format("-define(ENVIRONMENT,~p).~n",
				    [EnvironmentName]),
    Start = 
	io_lib:format(
	 % "start()->~n\tstart(1,~p,?NOTUNIQ). \n\n"++
	 % "start(Num) when is_number(Num)->\nstart(Num,~p,?NOTUNIQUE);\n\n"++
	 % "start(Name) when is_atom(Name)->\nstart(1,Name,?UNIQUE).\n\n"++
	  "start(AgName)->~n"++
	  %"\tAgName= utils:register_agent(Num,self(),Name,Uniqueness),~n"++
	  "\tAgent0 = reasoningCycle:"++
	  "start(AgName,~p,[],[],belief_base:start()),~n", 
	  [EnvironmentName]),
    Init = io_lib:format("\tAgent1 = add_initial_beliefs(Agent0),\n"++
			 "\tAgent2 = add_initial_goals(Agent1),\n"++
			 "\tAgent3 = add_rules(Agent2),\n"++
			 "\tAgent4 = add_plans(Agent3),\n"++
			 "\treasoningCycle:reasoningCycle(Agent4#agentRationale"++			 "{module_name=~p}).\n\n",[Name]),
			 io_lib:format("~s~s~s~s~s~s~s~s",
		  [Module,Export,Import,
		   NameMacro,InternalMacro,
		   EnvironmentMacro,Start,Init]).   


split_beliefs_rules(BelsAndRules)->
    split_beliefs_rules(BelsAndRules,[],[]).

split_beliefs_rules([],AccBels,AccRules)->
    {AccBels,AccRules};
split_beliefs_rules([Bel=#var{}|Rest],AccBels,AccRules) ->
    split_beliefs_rules(Rest,[Bel|AccBels],AccRules);
split_beliefs_rules([Rule=#unparsed_rule{}|Rest],AccBels,AccRules) ->
    split_beliefs_rules(Rest,AccBels,[Rule|AccRules]).




parse_agents([])->
    ok;
parse_agents([Name|Names]) when is_atom(Name)->
    parse_agents([{Name,?DEFAULTENVIRONMENT}|Names]);
parse_agents([{Name,EnvironmentName}|Names])->
    {ok,Tokens} = scanner:getTokens(io_lib:format("~p.asl",[Name])),

    %% io:format("INTERMEDIATE CODE:::::::::::::::::\n"),

    {ok,[BelsAndRules,InitGoals,Plans]} = 
	jasonGrammar:parse(lists:flatten(Tokens)),
    %% io:format("BelsAndRules: ~p~n",[BelsAndRules]),
    {Beliefs,Rules} = split_beliefs_rules(BelsAndRules),
    SBeliefs = generate_initial_beliefs(Beliefs),
    %% io:format("Rules: ~p~n",[Rules]),

    NewRules = group_rules(Rules),
       %% io:format("NewRules: ~p~n",[NewRules]),
    SRules = generate_rules(NewRules),
    SInitGoals = generate_initial_goals(InitGoals),
    
    %% io:format("PLANS: ~p~n",[Plans]),

    SPlans = generatePlans(Plans),
    %io:format("SPLANS: ~s~n",[SPlans]),
    AgentPart1 = generate_agent_start(Name,EnvironmentName),
    AgentPart2 = io_lib:format("~s~n~n~s~n~n~s~n~n~s~n~n",
			   [SBeliefs,SRules,SInitGoals,SPlans]),
    AgentPart3 = "",%sourceForTestHandlingPlan(),
    SAgent = io_lib:format("~s~s~n~s",
			   [AgentPart1,AgentPart2,AgentPart3]),
  %  io:format("AGENT: ~n~s~n~n",[SAgent]),
    AgentFileName = io_lib:format("~p.erl",[Name]),
    {ok, AgentFile} = file:open(AgentFileName,[raw,write]),
    file:write(AgentFile,SAgent),
    file:close(AgentFile),
    parse_agents(Names).


%% Groups rules with the same name
group_rules(Rules)->
    group_rules(Rules,[]).

group_rules([],Groups) ->
    Groups;
group_rules([Rule=
	     #unparsed_rule{head = #var{functor = {Name},
			       args = Args}}|Rest],
	    Groups) ->
    %%Arity = size(Args),
    Key = Name,
    NewGroups =  
	case lists:keytake(Key,1,Groups) of
	false ->
	    [{Key,[Rule]}|Groups];
	 {value,{Key,Value},OtherGroups}->
	    [{Key,[Rule|Value]}|OtherGroups]
	end,
    group_rules(Rest,NewGroups).

