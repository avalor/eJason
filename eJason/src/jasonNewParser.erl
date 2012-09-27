%% Copyright (c) 2012, Álvaro Fernández Díaz
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
%% WHETHER IN CONT RACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @author Álvaro Fernández Díaz
%% @copyright 2012 Álvaro Fernández Díaz
%% @doc Provides all the functions necessary to transform eJason intermediate
%%      code to Erlang.


-module(jasonNewParser).

-compile(export_all).

-include("parser.hrl").
-include("variables.hrl").
-include("macros.hrl").


-record(plan, {
	  trigger = #event{},
	  context = #binary_operation{},
	  formulas = [],
	  bindings = []}).








is_binary_operator(Operator)->
	lists:member(Operator,?BINARYOPERATORS).

is_atom_term(Term) ->
    lists:member(Term,?ATOMTERMS).

is_action(Param)->
    Param == action.


is_ground([]) ->
    true;		  
is_ground([Elem|List]) when is_record(Elem,var)->
    case Elem#var.is_ground of
	true ->
	    is_ground(List);
	false ->
	    false
    end;
is_ground([#predicate{%name = Name,
			     %arguments = Args,
			     %annotations = Annot,
			    is_ground = IG}|List])->
    case IG of 
	true ->
	    is_ground(List);
	false ->
	    false
   % case is_ground([Name]) of
%	false ->
%	     false;
%	true ->
%	    case is_ground(tuple_to_list(Args)) of
%		false ->
%		    false;
%		true ->
%		    is_ground(Args++List)
%	    end
    end;
is_ground([Atom|List]) when is_atom(Atom)->
    is_ground(List).


find_binding(_,[])->
    ?UNBOUNDVAR;
find_binding(VarName,[Var= #var{name=VarName}|_]) ->
    Var;
find_binding(VarName,[_|List]) ->
    find_binding(VarName,List).


is_bound_atom(_,[])->
    ?UNBOUNDVAR;
is_bound_atom(Atom,[Var= #var{bind=Atom}|_]) ->
    Var;
is_bound_atom(Atom,[_|List]) ->
    is_bound_atom(Atom,List).

is_already_in_bb(#var{timestamp=TS},BB)->
    lists:any(fun (X) -> TS == X#var.timestamp end,BB).
    

get_var_from_timestamp(TS,BB)->
    [Res] = lists:filter(fun (#var{timestamp = T}) ->
				 TS == T end, BB),
    Res.



pre_parse([], Bindings,PStruct)->
    NewPStruct = 
	case PStruct#parsing_struct.is_ordered of
	    true ->
		PStruct;
	    false ->
		NewAcc = lists:reverse(PStruct#parsing_struct.accum),
		PStruct#parsing_struct{
		  accum = NewAcc}
	end,
    {Bindings,NewPStruct};
pre_parse([true|Expr], Bindings,PStruct)->
    pre_parse_atom([{atom,0,true}|Expr],
				    Bindings,PStruct);
pre_parse([{var,_Line,VarName}|Expr],
	  Bindings,
	  PStruct= #parsing_struct{info=Info,counter = Counter})->


    {NewBindings,NewCode,NewCounter} = 
	case Info of %% the input parameters (plans,rules)
                     %% SHALL be given different names
	   []->
		case find_binding(VarName,Bindings) of
		    ?UNBOUNDVAR ->		
			NewVar = 
			    #var{name = VarName},
			{[NewVar|Bindings],NewVar,Counter};
		    Value ->
			{Bindings,Value,Counter}
		end;
	    params ->
		case find_binding(VarName,Bindings) of
		    ?UNBOUNDVAR ->		
			NewVar = 
			    #var{name = VarName},
			{[NewVar|Bindings],NewVar,Counter };
		    Value ->
			NewName =
			    list_to_atom(lists:flatten(
					   io_lib:format("~s~p",
							 [atom_to_list(
							    Value#var.name),
							  Counter]))),
			{Bindings,
			 Value#var{name = NewName},Counter+1}
		end
	end,
		
    Acc = PStruct#parsing_struct.accum,
	 pre_parse(Expr, NewBindings,
	  		            PStruct#parsing_struct{
				      accum = [NewCode|Acc]});
pre_parse([{AddStruct,Term}|Expr],
			   Bindings,PStruct)->
       Counter = PStruct#parsing_struct.counter,
	 {NewBindings,NewPStruct} = 
                  pre_parse([Term], Bindings,
			    #parsing_struct{
				    counter=Counter}),
        
    [NewTerm] = NewPStruct#parsing_struct.accum,
    Counter1 = NewPStruct#parsing_struct.counter,
    Acc =  PStruct#parsing_struct.accum,
    pre_parse(Expr, NewBindings,
			       PStruct#parsing_struct{
				 counter = Counter1,
				 accum = [{AddStruct,
					   NewTerm}|Acc]});
pre_parse(List=[{action,_Atype,_Package,_Body}|_],Bindings,PStruct)->
    pre_parse_action(List,Bindings,PStruct);
pre_parse(List=[{Head,_Elem1,_Elem2}|_],Bindings,PStruct)->
    Binary = is_binary_operator(Head),
    Atom = is_atom_term(Head),
    Action = is_action(Head),
    if
	
	Binary -> 
	    pre_parse_binary_operator(List,Bindings,PStruct);
	Atom -> 
	    pre_parse_atom(List,Bindings,PStruct);
	Action ->
	    pre_parse_action(List,Bindings,PStruct);
	
	true ->
	    io:format("ERROR: cannot parse ~p~n",[Head]),
	    exit(parsing_error)
    end;
pre_parse([{formula,FunName,Terms,Label}|Expr],
			   Bindings,PStruct)->
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
    %io:format("Terms: ~p~n", [Terms]),
    {NewBinding2,NewPStruct2} =
	pre_parse(Terms, NewBinding1,
				   #parsing_struct{
					    info = params,
					    counter=Counter2}),

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
    %io:format("NewTerms: ~p~n", [NewTerms]),
    NewLabel = NewPStruct3#parsing_struct.accum,
    Predicate = make_predicate(NewAtom,NewTerms,NewLabel),

    pre_parse(Expr, NewBinding3,
			       PStruct#parsing_struct{
				 counter = Counter4,
				 accum = [Predicate|Acc]});


pre_parse(Other,_,_) ->
    io:format("ERROR: cannot parse ~p~n",[Other]),
    exit(parsing_error).



pre_parse_atom([{_,_Line,AtomValue}|Expr],
			   Bindings,PStruct)->
    Counter = PStruct#parsing_struct.counter,
    Info = PStruct#parsing_struct.info,
	 {NewBindings,NewCode,NewCounter} =
	case Info of 
	    []->
		case is_bound_atom(AtomValue,Bindings) of
		    ?UNBOUNDVAR->
			SVarName ="EjasonVar"++integer_to_list(Counter),
			VarName = list_to_atom(SVarName),
			Counter2 = Counter +1,
			NewVar = 
			    #var{name = VarName,
                                     is_ground = true,
				 bind = AtomValue},
			{[NewVar|Bindings],NewVar,Counter2};
		    Value ->
			{Bindings,Value,Counter}
		end;
	    params ->
		SVarName ="EjasonVar"++integer_to_list(Counter),
		VarName = list_to_atom(SVarName),
		Counter2 = Counter +1,
		case is_bound_atom(AtomValue,Bindings) of
		    ?UNBOUNDVAR->
			NewVar = 
			    #var{name = VarName,
                                     is_ground = true,
				 bind = AtomValue},
			{[NewVar|Bindings],NewVar,Counter2};
		    Value ->
			{Bindings,Value#var{name=VarName},
			 Counter2}
		end
	end,
    Acc = PStruct#parsing_struct.accum,
    pre_parse(Expr, NewBindings,
	      PStruct#parsing_struct{
		counter = NewCounter,
		accum = [NewCode|Acc]}).



pre_parse_binary_operator([{Operator,Left,Right}|Expr],
			   Bindings,PStruct)->
%    io:format("~p~n",[{Operator,Left,Right}]),
    Counter1 = PStruct#parsing_struct.counter,
    {NewBinding1,NewPStruct1} =
	pre_parse([Left], Bindings,
		  #parsing_struct{
					   %is_ordered = true,
			  counter=Counter1}),
    Counter2 = NewPStruct1#parsing_struct.counter,
    {NewBinding2,NewPStruct2} =
	pre_parse([Right], NewBinding1,
				   #parsing_struct{
					    %is_ordered = true,
					    counter=Counter2}),
    Acc = PStruct#parsing_struct.accum,
    Counter3 = NewPStruct2#parsing_struct.counter,
    NewLeft = NewPStruct1#parsing_struct.accum,
    NewRight = NewPStruct2#parsing_struct.accum,

    NewOperation = #binary_operation{
      operator= Operator,
      left_part = NewLeft,
      right_part = NewRight},

    pre_parse(Expr, NewBinding2,
	      PStruct#parsing_struct{
		counter = Counter3,
		accum = [NewOperation|Acc]}).



pre_parse_action([{action,AType,Term}|Expr],
			   Bindings,PStruct)->
    pre_parse_action([{action,AType,"",Term}|Expr],
			   Bindings,PStruct);
pre_parse_action([{action,AType,Package,Term}|Expr],
			   Bindings,PStruct)->
    Counter = PStruct#parsing_struct.counter,
    {NewBindings,NewPStruct} = 
	pre_parse([Term],Bindings,
		  #parsing_struct{
			 counter=Counter}),
    
    NewTerm = NewPStruct#parsing_struct.accum,
    Counter1 = NewPStruct#parsing_struct.counter,
    
    NewAction = #action{type = AType,
			package = Package,
			body = NewTerm},
    Acc =  PStruct#parsing_struct.accum,
    pre_parse(Expr, NewBindings,
			       PStruct#parsing_struct{
				 counter = Counter1,
				 accum = [NewAction|Acc]}).






make_predicate(Name,Arguments,Annotations)->   
   % io:format("Name: ~p~nArgs: ~p~nAnnot: ~p~n",
%	      [Name,Arguments,Annotations]),
    NewArgs = wrap_as_variables(Arguments),
    P = #predicate{name = Name,
		   arguments = list_to_tuple(NewArgs),
		   annotations = Annotations,
		   is_ground = is_ground([Name]++NewArgs
					 ++Annotations)},
%    io:format("Predicate: ~p~n",[P]),
    P.


make_belief(F={formula,_Atom,_Arguments,_Annotations})->
    {_Bindings,PStruct} = pre_parse([F],[],#parsing_struct{}),
    [Predicate] = PStruct#parsing_struct.accum,
   % The bindings are added to the record because beliefs are 
   % standalone structs.
    Predicate.


% Used to turn a predicate in an argument into a bound variable.
wrap_as_variables(Terms)->
    wrap_as_variables(Terms,[]).

wrap_as_variables([],Acc)->
    lists:reverse(Acc);
wrap_as_variables([Var = #var{}|List],Acc) ->
    wrap_as_variables(List,[Var|Acc]);
wrap_as_variables([Pred = #predicate{%name = Name,
				    arguments = _Args,
				    annotations = _Annot}|List],Acc) ->   

    %NewVar = #var{name= Name#var.name, bind = Pred},
    wrap_as_variables(List,[Pred|Acc]).



%% Receives a formula and generates a rule
make_rule({F={formula,_Atom,_Arguments,_Annotations},
	  eJasonRule,Conditions})->
%    io:format("Formula for Rule: ~p~n",[F]),

    {Bindings,PStruct} =
	pre_parse([F],[],#parsing_struct{}),
    
    Counter = PStruct#parsing_struct.counter,

    {Bindings2,PStruct2} =
	pre_parse(Conditions,Bindings,#parsing_struct{
						counter = Counter}),
    
    [Head] = PStruct#parsing_struct.accum,
    Body = PStruct2#parsing_struct.accum,
%    io:format("RuleHead : ~p~nBindings: ~p~n",[Head,Bindings2]),
    #rule{head = Head,
	  body = Body,
	  bindings = Bindings2}.

make_event(EventType, Body) ->
    
    {Bindings,PStruct} =
	pre_parse([Body], [],  #parsing_struct{}),

    [Predicate] = PStruct#parsing_struct.accum,
    #event{type = EventType, 
	   body = Predicate#predicate{bindings=Bindings}}.
       

make_plan(Trigger,Context,Body)->
    %io:format("Trigger: ~p~nContext: ~p~nBody: ~p~n",
%	      [Trigger,Context,Body]),

    {Bindings,PStruct} =
	pre_parse([Trigger],[],#parsing_struct{}),

    Counter = PStruct#parsing_struct.counter,
    
    {Bindings2,PStruct2} =
	pre_parse([Context],Bindings,#parsing_struct{
					     is_ordered = true,
						counter = Counter}),

    Counter2 = PStruct2#parsing_struct.counter,
    
    {Bindings3,PStruct3} =
	pre_parse(Body,Bindings2, #parsing_struct{
					  counter = Counter2}),
    %TODO: maybe a make_event is more elegant
    [{TriggerType,Event}] = PStruct#parsing_struct.accum,
    [NewContext] = PStruct2#parsing_struct.accum,
    NewBody = PStruct3#parsing_struct.accum,
    
    #plan{trigger = #event{type = TriggerType,
			   body = Event},
	  context = NewContext,
	  formulas = NewBody,
	  bindings = Bindings3}.




%% Converts an external action into an internal action after finding a dot
parseInternalAction({no_package},{action, external_action,F}) ->
    {action,internal_action,"'.'",F};
parseInternalAction({package,P},{action,external_action,F}) ->
    {action,internal_action,io_lib:format("~p.",[P]),F};
parseInternalAction({no_package},A={action,internal_action,_,_}) ->
    A;%%Duplicated dot
parseInternalAction({package,P},{action,internal_action,Packages,F}) ->
   {action,internal_action,io_lib:format("~p'.'~s",[P,Packages]),F}.







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PARSING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




generateInitialBeliefs(Beliefs)->
 %   io:format("CALLED WITH PARAMS ~n~p~n",[Beliefs]),
       
    BeliefStrings = lists:map(fun jasonNewParser:sourceForInitialBelief/1,
			      Beliefs),

    Header = "%% Function to includeInitial beliefs.\n\n"++
	"addInitialBeliefs(Agent = #agentRationale{})->\n",

    Body = string:join(BeliefStrings,",\n"), 

    io_lib:format(
      "~s~n"++
      "\treasoningCycle:applyChanges(Agent,[\n~s]).",
      [Header, Body]).
        

sourceForInitialBelief(Belief=#predicate{}) ->
    InitialBelief = utils:predicate_to_tuple(Belief),
    NewEvent = #event{type = ?ADDBELIEF, body = InitialBelief},
    io_lib:format("\t\t~p",[NewEvent]).
 

sourceForRules(Rules) ->
    sourceForRules(Rules,{1,""}).

sourceForRules([],{_RULENUMBER,Acc}) ->
    Acc;
sourceForRules([{{Name,Arity},Rules}|Rest],{RuleNumber, Acc}) ->
    %io:format("Rules together: ~p~n",[Rules]),
    NumRules = length(Rules),
    SRulePosition = "_rule"++integer_to_list(RuleNumber)++"_",
    SRuleParams = source_many_vars("RuleParam",Arity),
    SRuleFunNames = string:join([io_lib:format("fun ~s~sinit_~p/~p",
		     [atom_to_list(Name),SRulePosition,X,Arity+2])
		     || X <- lists:seq(1,NumRules)],", "),	     
    
   
    SInitFun = io_lib:format("~p(BB,Bindings,~s)->\n"++
		  "ItFun = iterator:create_iterator([~s]) ,\n"++
		  "Fun = fun (X) -> apply(X,[BB,Bindings,~s]) end,\n"++
		  "iterator:create_iterator_fun(ItFun,Fun).\n\n",
		  [Name,SRuleParams,SRuleFunNames,SRuleParams]),
    SRules =
	string:join(
	  [sourceForRule(Rule,SRulePosition,Counter) ||
	      {Rule,Counter} <- lists:zip(Rules,lists:seq(1,NumRules))],
	  "\n"),
    sourceForRules(Rest,{RuleNumber+1,Acc++ SInitFun++SRules}).



sourceForRule(Rule =
	      #rule{head = Head, body = Body, bindings = Bindings},
	      SRulePosition,Counter) ->
   % io:format("Rule to source: ~p~n",[Rule]),
    FunName= list_to_atom(atom_to_list((Head#predicate.name)#var.bind)++
			  SRulePosition++"init_"++ 
			  integer_to_list(Counter)),
    sourceForInferenceStruct({rule,Bindings},Head,Body,
			     "Ejason_",[FunName,FunName]).

%% Derived from SourceForRule.
%% Used to generate both rules and trigger+context for plans.
%% Header is a record predicate
%% Body is a binary_operation record
%% TODO: design two versions for RULES & PLANS.
sourceForInferenceStruct(PLANORRULE,Header, Body,
	     ArgsPrefix,[HeaderFunName,BodyFunName]) -> 

   % FunName = (Header#predicate.name)#var.bind,
%   io:format("Header: ~p~n",[Header]),
    Params = tuple_to_list(Header#predicate.arguments),
	%Header#predicate.annotations,
    PSArgs = args_to_string(Params,ArgsPrefix),
 %   io:format("Params: ~p~n",[Params]),

    {SArguments,NewParams, NewVariables} =
	PSArgs#parsing_struct.info,
    SFunName =  
	case PLANORRULE of 
	    plan ->
		io_lib:format("~s(BB,Bindings0,~s,Label)",[HeaderFunName,SArguments]);
	    {rule,_} ->
		io_lib:format("~s(BB,Bindings,~s)",[HeaderFunName,SArguments])
	end,
   
   
       
   %io:format("Head: ~p~n",[Header]),
   % io:format("Body: ~p~n",[Body]),
   %io:format("Predicate: ~s->~n",[SFunName]),

    NumParams = length(NewParams),
    SParamMatches = 
	case PLANORRULE of 
	    plan -> 
		source_param_matches(NewParams);
	    {rule,RuleBindings} ->
		SBindings0 =  io_lib:format("\tBindings0 = Bindings ++ ~p,\n",
				[RuleBindings++NewVariables]),
		SBindings0 ++ source_param_matches(NewParams)
	end,
    
    
    SMatchAnnotations =
	source_for_annotations(Header#predicate.annotations),
      

   %% Defines which are the params in the header function
    SParamsGroup = 
	io_lib:format("\tParams = [~s],\n",
		      [source_many_vars("Param",NumParams)]), 
	%    io:format("SParamsGroup: ~s~n",[SParamsGroup]),


    PStructBody = source_for_conditions(BodyFunName,Body),
%    io:format("Info: ~p~n",[PStructBody#parsing_struct.info]),


    {FirstFun,_} = 
	PStructBody#parsing_struct.info,


    SIterator = 
	case   PLANORRULE of 
	    plan->
		    
	io_lib:format("Fun = fun (X) -> ~p_~p(BB,Params,X) end,\n"++
		      "\tItList = iterator:create_iterator([Bindings~p]),\n"++
		      "\titerator:create_iterator_fun(ItList,Fun)\n\n\n",
		      [BodyFunName,FirstFun,NumParams+1]);
	    {rule,_}->
		io_lib:format("Fun = fun (X) -> ~p_~p(BB,Params,X) end,\n"++
		      "\tItList = iterator:create_iterator([Bindings~p]),\n"++
		      "\titerator:create_iterator_fun(ItList,Fun)\n\n\n",
		      [BodyFunName,FirstFun,NumParams])
	end,

    STryCatch = 
	case  
	    PLANORRULE of 
	    plan -> 
		io_lib:format("\ttry\n\n~s\n"++
			      "case  variables:try_annot_matches(Bindings~p,\n"++
		      "\t\tLabel,~s) of\n\tfalse -> false;\n\tBindings~p->\n"
			      "\t~s\n\n\t~s\n\tend\n\n\tcatch\n\t\texit"++
		      ":param_does_not_match ->\n\t\t\tfalse\n\tend.\n\n",
			      [SParamMatches]++
			      [NumParams, SMatchAnnotations,NumParams+1]++
		      [SParamsGroup,SIterator]);
	    {rule,_} ->

		io_lib:format("\ttry\n\n~s\n"++
		      "\t~s\n\n\t~s\n\n\tcatch\n\t\texit"++
		      ":param_does_not_match ->\n\t\t\tfalse\n\tend.\n\n",
		      [SParamMatches]++
		      [SParamsGroup,SIterator])
	end,
	
    
    SHeadRule = io_lib:format("~s->\n~s",
			  [SFunName,STryCatch]),
    
    SConditions = PStructBody#parsing_struct.accum,

  %  io:format("SRULE: ~s~n",[SHeadRule]),
  %  io:format("SConditions: ~s~n",[SConditions]),
    SHeadRule ++ SConditions.
    


%% Generates the proper set of condition functions following
%% the corresponding conditions-tree
%% Returns a parsing_struct where the "info" field indicated the
%% function in the root of the tree
source_for_conditions(FunName,Body)->
    Acc =  io_lib:format(
	     "~p_0(_BB,Params,Bindings)->\n"++
	     "{Params,Bindings}.\n\n",
		 %"\tvariables:return_params(Params,Bindings).\n\n",
		 [FunName]),
    source_for_conditions(FunName,Body,  #parsing_struct{counter = 1,
							 info = {0,2},
							accum = Acc}).



source_for_conditions(FunName,[#binary_operation{operator=log_and,
					       left_part = Left,
					       right_part = Right}],
		     #parsing_struct{counter = Counter,accum = Acc,
				     info = {Return,NextFree}}) ->

    ReturnLeft = find_next(Left,Right, {Counter,NextFree}),
    PSLeft = 
	source_for_conditions(FunName,Left,  
			      #parsing_struct{counter = NextFree,
					      info = {ReturnLeft,NextFree+1}}),
    LeftNextFree = PSLeft#parsing_struct.counter,
%    io:format("LNF: ~p~n",[LeftNextFree]),
    AccLeft = PSLeft#parsing_struct.accum,
    

    PSRight = 
	source_for_conditions(
	  FunName,Right,  
	  #parsing_struct{counter = Counter,
			  info = {Return,LeftNextFree}}),
    RightNextFree = PSRight#parsing_struct.counter,
    AccRight = PSRight#parsing_struct.accum,
    

    #parsing_struct{counter = RightNextFree, info={NextFree,RightNextFree+1},
		   accum = Acc++AccLeft++AccRight};
  
source_for_conditions(FunName,[#binary_operation{operator=log_or,
					       left_part = Left,
					       right_part = Right}],
		     #parsing_struct{counter = Counter,accum = Acc,
				     info = {Return,NextFree}}) ->

    
    PSLeft = source_for_conditions(FunName,Left,  #parsing_struct{counter = NextFree,
								  info = {Return,NextFree+1}}),
    LeftNextFree = PSLeft#parsing_struct.counter,
    AccLeft = PSLeft#parsing_struct.accum,
    

    PSRight = 
	source_for_conditions(FunName,Right,
			      #parsing_struct{counter = LeftNextFree,
					      info = {Return,LeftNextFree+1}}),
    RightNextFree = PSRight#parsing_struct.counter,
    AccRight = PSRight#parsing_struct.accum,
    
    HeadNewFun =
	io_lib:format("~p_~p(BB,Params,Bindings)->\n",
		      [FunName,Counter]),

    BodyNewFun =
	io_lib:format(
	  "\tFunIterator = iterator:create_iterator([fun () ~p_~p(BB,Params,Bindings) end,"++
	  "fun () ~p_~p(BB,Params,Bindings) end]),\n\tFun = fun(F) -> F() end, \n"++
	  "iterator:create_iterator_fun(FunIterator,Fun).\n\n\n",
		      [FunName,NextFree, FunName,LeftNextFree]),
    NewFun = HeadNewFun ++ BodyNewFun,


    #parsing_struct{counter = RightNextFree, info={Return,RightNextFree+1},
		   accum = string:join([Acc,AccLeft,AccRight,NewFun],"")};

source_for_conditions(FunName, [#predicate{name = Name,arguments=Args}],
		     #parsing_struct{counter=Counter, info={Return,NextFree},
				    accum = Acc}) ->
    SFunName =  io_lib:format("~p_~p(BB,Params,Bindings)->\n",
			      [FunName,Counter]),
    
    VarsArgs = utils:gather_all_vars(tuple_to_list(Args)),
%io:format("VarsArgs: ~p~nArgs: ~p~n",[VarsArgs,Args]),
%    io:format("~p(~p)~n",[Name,Args]),
    VarTSTupleList = 
	[ {Var,TS}|| {Var,TS} 
			 <- lists:flatten(
			      lists:map(fun(#var{name=N,timestamp=T})
					   ->
						{N,T} end,VarsArgs))
			],
%       io:format("2~n"),

    %{VarList,_TSList} = lists:unzip(VarTSTupleList),
    
    {SFindPredName, SPredName}  = 
	case Name#var.is_ground of
	    true ->
		{"",Name#var.bind};
	    false ->
		{io_lib:format(
		  "\tEjasonPredName = variables:find(Bindings,~p),\n",
			      [Name#var.timestamp]),
		 'EjasonPredName'}
	end,
    SVariableFind = string:join(
	[io_lib:format("\t~s = variables:find(Bindings,~p),\n",
		       [atom_to_list(Var),TS]) ||
	    {Var,TS}<- VarTSTupleList], ""),
    
    SArgs = 	
	prepare_query(tuple_to_list(Args)),
    
    SCase = 
	io_lib:format(
	  "\n\tcase belief_base:query_bb(BB,Bindings,?MODULE,{~s,"++
	  "{~s},[]}) of\n"++
	  "\t\tfalse -> \n \t\t\tfalse;\n\t\tIt when is_function(It) ->\n"++
	  "\t\t\tFun = fun (NewBindings) -> \n "++
	  "\t\t\t\tFinalBindings = variables:update(Bindings,NewBindings),\n"++
	  "\t\t\t\t?MODULE:~p_~p(BB,Params,FinalBindings)end,\n"++
	  "\t\t\t\titerator:create_iterator_fun(It,Fun)\n\tend.\n\n\n",
	  [SPredName,SArgs,FunName,Return]),
    
    SCondition = SFunName ++ SFindPredName ++ SVariableFind ++ SCase,
    #parsing_struct{counter = NextFree,
		    accum = Acc++SCondition,
		   info = {Counter,NextFree}};

source_for_conditions(FunName,[BO = #binary_operation{}],
		      #parsing_struct{counter = Counter,accum = Acc,
				      info = {Return,NextFree}}) ->
   % io:format("BO: ~p~n",[BO]),
    SFunName =  io_lib:format("~p_~p(BB,Params,Bindings)->\n",
			      [FunName,Counter]),

    VarsInBO = utils:erase_repeated_vars(find_vars(BO)),
    %io:format("Vars in BO: ~p~n",[VarsInBO]),



    VarTSTupleList = lists:map(fun (#var{name=N,timestamp=T})->
				       {N,T} end, VarsInBO),
     
    {VarList,_TSList} = lists:unzip(VarTSTupleList),
        
    SVariableFind = string:join(
	[io_lib:format("\t~s = variables:find(Bindings,~p),\n",
		       [atom_to_list(Var),TS]) ||
	    {Var,TS}<- VarTSTupleList], ""),
    
    SConditionFun = fun_for_binary_operation(BO),
    SCase = io_lib:format(
	      "\tcase ~s of \n"++
	      "\t\tfalse ->\n\t\t\tfalse;\n"++ %% false
	      "\t\ttrue -> \n"++ %true
	      "\t\t\tIt = iterator:create_iterator([Bindings]),\n"++
	      "\t\t\tFun = fun (NewBindings)-> ~p_~p(BB,Params,NewBindings)end,\n"++
	      "\t\t\titerator:create_iterator_fun(It,Fun);\n"++
	      "\t\tVar when is_record(Var,var) -> \n"++ % when is a var
	      "\t\t\tNewBindings = variables:update(Bindings,[Var]),\n"++
	      "\t\t\tIt = iterator:create_iterator([NewBindings]),\n"++
	      "\t\t\tFun = fun (EjasonParserX)-> ~p_~p(BB,Params,EjasonParserX)end,\n"++
	      "\t\t\titerator:create_iterator_fun(It,Fun)\n\tend.\n\n\n",
	      [SConditionFun,FunName,Return,FunName,Return]),

    SCondition = SFunName ++ SVariableFind ++ SCase,
    #parsing_struct{counter = NextFree,
		    accum = Acc++SCondition,
		    info = {Counter,NextFree}};



source_for_conditions(FunName, [{log_not,#predicate{name = Name,arguments=Args}}],
		      #parsing_struct{counter=Counter, info={Return,NextFree},
				      accum = Acc}) ->
    SFunName =  io_lib:format("~p_~p(BB,Params,Bindings)->\n",
			      [FunName,Counter]),
    
    VarsArgs = utils:gather_all_vars(tuple_to_list(Args)),

    VarTSTupleList = 
	[ {Var,TS}|| {Var,TS} 
			 <- lists:flatten(
			      lists:map(fun(#var{name=N,timestamp=T})
					   ->
						{N,T} end,VarsArgs))
			],   
    {SFindPredName, SPredName}  = 
	case Name#var.is_ground of
	    true ->
		{"",Name#var.bind};
	    false ->
		{io_lib:format(
		  "\tEjasonPredName = variables:find(Bindings,~p),\n",
			      [Name#var.timestamp]),
		 'EjasonPredName'}
	end,
    SVariableFind = string:join(
	[io_lib:format("\t~s = variables:find(Bindings,~p),\n",
		       [atom_to_list(Var),TS]) ||
	    {Var,TS}<- VarTSTupleList], ""),
    
    SArgs = 	
	prepare_query(tuple_to_list(Args)),
    
    SCase = 
	io_lib:format(
	  "\n\tIt = belief_base:query_bb(BB,Bindings,?MODULE,{~s,"++
	  "{~s},[]}), \n\t case It() of\n"++
	  "\t\tfalse -> \n"++
	  "\t\t\t\t?MODULE:~p_~p(BB,Params,Bindings);\n"++

	  "\n\t\t _ ->\n"++
	  "\t\t\t\t false end. \n\n\n", % query matches, then return false

	  [SPredName,SArgs,FunName,Return]),
    
    SCondition = SFunName ++ SFindPredName ++ SVariableFind ++ SCase,
    

#parsing_struct{counter = NextFree,
		    accum = Acc++SCondition,
		   info = {Counter,NextFree}};



%source_for_conditions(FunName,[{log_not,Negated}],
%		      #parsing_struct{counter = Counter,accum = Acc,
%				      info = {Return,NextFree}}) ->




 %   SFunName =  io_lib:format("~p_~p(BB,Params,Bindings)->\n",
%			      [FunName,Counter]),

 %   VarsInNot = utils:erase_repeated_vars(find_vars(Negated)),

  %  VarTSTupleList = lists:map(fun (#var{name=N,timestamp=T})->
%				       {N,T} end, VarsInNot),
    
       
 %   SVariableFind = string:join(
%	[io_lib:format("\t~s = variables:find(Bindings,~p),\n",
%		       [atom_to_list(Var),TS]) ||
%	    {Var,TS}<- VarTSTupleList], ""),
 %   io:format("Negated: ~p~n",[Negated]),
  %  io:format("SVariableFind: ~s  ~n",[SVariableFind]),
%
 %   SConditionFun = fun_for_binary_operation({log_not,Negated}),
  %  io:format("SConditionFun: ~p~n",[SConditionFun]),
   % SCase = io_lib:format(
%	      "\tcase ~s of \n"++
%	      "\t\tfalse ->\n\t\t\tfalse;\n"++ %% false
%	      "\t\ttrue -> \n"++ %true
%	      "\t\t\tIt = iterator:create_iterator([Bindings]),\n"++
%	      "\t\t\tFun = fun (NewBindings)-> ~p_~p(BB,Params,NewBindings)end,\n%"++
%	      "\t\t\titerator:create_iterator_fun(It,Fun);\n"++
%	      "\t\tVar when is_record(Var,var) -> \n"++ % when is a var
%	      "\t\t\tNewBindings = variables:update(Bindings,[Var]),\n"++
%	      "\t\t\tIt = iterator:create_iterator([NewBindings]),\n"++
%	      "\t\t\tFun = fun (EjasonParserX)-> ~p_~p(BB,Params,EjasonParserX)en%d,\n"++
%	      "\t\t\titerator:create_iterator_fun(It,Fun)\n\tend.\n\n\n",
%	      [SConditionFun,FunName,Return,FunName,Return]),
%
 %   SCondition = SFunName ++ SVariableFind ++ SCase,
  %  #parsing_struct{counter = NextFree,
%		    accum = Acc++SCondition,
%		    info = {Counter,NextFree}};

source_for_conditions(_FunName,[#var{bind = true}],
		      PS) ->
    PS;
source_for_conditions(FunName,Expression,_) ->
    io:format("Problems found parsing logical conditions for the plan or rule"++
	      "~p (ie: name_ordernum_partofplan) Please, try adding some "++
	      "parenthesis to simplify parsing.",[FunName]),
    exit({error,{jasonNewParser,source_for_conditions,
		 [FunName,Expression]}}).
			     






%% Parses the input parameters for a query
prepare_query(#var{name = Name})->
    R = io_lib:format("~s",[Name]),
%    io:format("R = ~s~n",[R]),
    R;
prepare_query(#predicate{name = Name, arguments = Args}) ->
    R = io_lib:format("{~s,{~s},[]}",[prepare_query(Name),
				    prepare_query(tuple_to_list(Args))]),
 %   io:format("R = ~p~n",[R]),
    R;
prepare_query(List) when is_list(List) ->
    Fun = fun (X)->
		  prepare_query(X) end,
    R  =string:join(lists:map(Fun,List),", "),
  %  io:format("R = ~p~n",[R]),
    R.


find_next([#binary_operation{operator=log_and,
			     left_part = Left}],
	  Right,
	  {_Counter,NextFree}) ->
    find_next(Left,Right,{NextFree,NextFree+1});
find_next(_,Right, {Counter,NextFree})->
    find_next(Right,{Counter,NextFree+1}).




find_next([#binary_operation{operator=log_and,
			     left_part = Left}],
	  {_Counter,NextFree}) ->
    find_next(Left,{NextFree,NextFree+1});
find_next(_,{Counter,_NextFree}) ->
    Counter.


fun_for_binary_operation(#binary_operation{operator = Operator,
					  left_part = [Left],
					  right_part = [Right]}) ->

    io_lib:format(" utils:~p(~s,~s) ",
		  [Operator,fun_for_binary_operation(Left),
		   fun_for_binary_operation(Right)]);
fun_for_binary_operation({log_not,Predicate}) ->

    io_lib:format(" utils:log_not (~s) ",
		  [fun_for_binary_operation(Predicate)]);
fun_for_binary_operation(#predicate{name = Name,arguments = {}}) ->
    atom_to_list(Name#var.name);
fun_for_binary_operation(P = #predicate{name = Name,arguments = Args}) ->
    io:format("P: ~p~n",[P]),
    Fun = fun (X) -> fun_for_binary_operation(X) end,
    io_lib:format("~s(~s)",[Name#var.name,
			    string:join(
			      lists:map(Fun,tuple_to_list(Args)),
			      ", ")]);
fun_for_binary_operation(#var{name = Name}) ->
    atom_to_list(Name).


%% Returns the variables involved in a binary operation
find_vars(#binary_operation{left_part=[Left],right_part=[Right]})->
%    io:format("B: ~p~n",[B]),
    find_vars(Left) ++ find_vars(Right);
%    variables:update(find_vars(Left), find_vars(Right));
find_vars(#predicate{name = Name, arguments = Args,annotations = Annot}) ->
    Fun = fun(X) -> find_vars(X) end,
    lists:flatten(lists:map(Fun,[Name|tuple_to_list(Args)]++Annot));
find_vars(Var = #var{bind = Bind}) ->
    [Var|find_vars(Bind)];
find_vars(_Else) ->
    [].


generateInitialGoals(Goals)->
    %io:format("InitGoals: ~p~n",[Goals]),
    GoalStrings = lists:map(fun jasonNewParser:sourceForInitialGoal/1,
			      Goals),

    Header = "%% Function to include all initial goals.\n\n"++
	"addInitialGoals(Agent = #agentRationale{})->\n",

    Body = string:join(GoalStrings,",\n"), 

    Res = io_lib:format(
	    "~s~n"++
	    "\treasoningCycle:applyChanges(Agent,[\n~s]).",
	    [Header, Body]),
%    io:format("InitialGoals: ~s~n",[Res]),
    Res.


sourceForInitialGoal(Event = #event{body = Body}) ->
    {Name,Args,Annot} =  utils:predicate_to_vars_tuple(Body),
%    io:format("JNP Goal Body: ~p~nTurned into: ~p~n",
%	      [Body, {Name,Args,Annot}]),
    
   io_lib:format("\t\t~p",[Event#event{body =
					{Name,add_achievement_goal,
					 Args,Annot}}]).



generatePlans(Plans) ->
%    io:format("Plans: ~p~n",[Plans]),
    PStructPlans = sourceForPlans(Plans,#parsing_struct{counter = 1,
						       accum = "",
						       info = []}),
    SPlans = PStructPlans#parsing_struct.accum,
    SPlanRecords = PStructPlans#parsing_struct.info,
  %    io:format("SourceForPlans PLANS: ~s~n",[SPlanRecords]),

    SAddPlans =
	io_lib:format("\naddPlans(Agent = #agentRationale{})->\n"++
		      "\tAgent#agentRationale{plans = [~s,\n"++
		      "\t#plan{trigger=fun test_goal:trigger/6,~n"++
		      "\tcontext=fun test_goal:context/6,\n"++
		      "\tbody=[fun test_goal:formula/1],\n"++
		      "\tbindings= []}\n"++
		      "\t]}.\n\n",
		      [string:join(SPlanRecords,",\n")]),
    

%    io:format("SPlans: ~s~n",[SPlans]),
    SAddPlans ++ SPlans.


sourceForPlans([],PStruct) ->
    PStruct#parsing_struct{info = lists:reverse(PStruct#parsing_struct.info)};
sourceForPlans([PlanAhora=#plan{trigger = #event{type=Type,body=TBody}, context = Context,
		      formulas = Formulas, bindings = Bindings}
		|Plans],#parsing_struct{counter = Counter,
					accum = Acc,
					info = Info})->
%    io:format("PlanAhora: ~p~n",[PlanAhora]),

    TypePlanVar = #var{name = 'EjasonEventType',
		       bind = Type, is_ground=true} ,

    PlanNameVar = #var{name = 'EjasonPlanName',
		      bind = (TBody#predicate.name)#var.bind,
		       is_ground = true},
    
    

    TBodyParams = TBody#predicate.arguments,
    SFunName = 
	atom_to_list((TBody#predicate.name)#var.bind)++"_"++
	integer_to_list(Counter),
    HeaderName = list_to_atom(SFunName++"_trigger"),
    BodyName = list_to_atom(SFunName++"_context"),
    %% Adding event-type as second argument
    TriggerParams = 
	list_to_tuple([PlanNameVar,TypePlanVar]++
		      case TBodyParams of 
			  {} ->
			      [];
			  _ ->
			      tuple_to_list(TBodyParams)
		      end),
    STriggerContext =
	sourceForInferenceStruct(plan,
	  TBody#predicate{arguments = TriggerParams},
	  [Context],
	  "Ejason_",
	  [HeaderName,BodyName]),

    PSFormulas = 
	sourceForPlanFormulas(Formulas,
			      #parsing_struct{counter = 1,
					     accum = "",
					     info = SFunName}),
    SFormulas = PSFormulas#parsing_struct.accum,
%	io_lib:format("~s_last_formula(Params,Bindings)->\n"++
%		     "\tutils:get_params("),


%    PlanBindings = PSFormulas#parsing_struct.info,

    SPlanRecord = generatePlanRecord(SFunName,
				     size(TriggerParams),
				     length(Formulas),
				    [PlanNameVar,TypePlanVar]++Bindings),
    


    %io:format("~n~nTriggerContext: ~s~n",[STriggerContext]),
    sourceForPlans(Plans,
		   #parsing_struct{counter = Counter+1,
				   accum = Acc++STriggerContext++SFormulas,
				   info = [[SPlanRecord]|Info]}).
    



generatePlanRecord(SPlanName, NumParams,NumFormulas,Bindings)->
    SFunFormulas = 
	string:join(
	  [io_lib:format("fun ?Name:~s_body_formula_~p/1",
			 [SPlanName, X])|| 
	      X <- lists:seq(1,NumFormulas)]% ++ 
	%  [io_lib:format("fun ?Name:~s_last_formula/1")]
	  ,
	  ",\n"),
      
      
    io_lib:format(
      "\t#plan{trigger=fun ?Name:~s_trigger/~p,~n"++
      "\tcontext=fun ?Name:~s_context/2,\n"++
      "\tbody=[~s],\n"++
      "\tbindings= ~p}",
      [SPlanName,NumParams+3,SPlanName,SFunFormulas,Bindings]).





%% TODO: check whether creating new formulas is necessary or it suffices
%%  to create new plan records with anonym calls to utils:ActionType.
sourceForPlanFormulas([],PStruct)->
    PStruct;
sourceForPlanFormulas([#var{bind = true}| Formulas],
		     PS = #parsing_struct{}) ->
    sourceForPlanFormulas(Formulas,PS);    
sourceForPlanFormulas([Ac = #action{type = Type,package=_Package,body = [Body]}|
		      Formulas], #parsing_struct{counter = Counter,
						accum = Acc,
						info = SFunName}) ->
%    io:format("Plan-Body Action: ~p~n",[Ac]),
    SBody = sourceForPredicate(Body),
    
    {NewType,FirstArg,SecondArg}  =
	case Type of
	    'fun' ->
%		io:format("Body: ~p~n",[Body]),
		{operation,%Body#binary_operation.operator,
		 io_lib:format("~p, utils:valuate(Bindings,~p)",
			       [Body#binary_operation.operator,
				SBody#binary_operation.left_part]),
		 io_lib:format("utils:valuate(Bindings,~p)",
			       [SBody#binary_operation.right_part])};
		 
	    _ ->
		{Type,
		 "?MODULE",
		 io_lib:format("utils:valuate(Bindings,~p)",[SBody])}
	end,

    SFormula = 
%	case NewType of
%	    rel_assig ->
%		io_lib:format("~s_body_formula_~p(Bindings)->\n"++
%			 "utils:~p(,utils:valuate(Bindings,~p)).\n\n",
%			      [SFunName,Counter]++
%			      [NewType,SBody]),
	    


	io_lib:format("~s_body_formula_~p(Bindings)->\n"++
		      "utils:~p(~s,~s).\n\n",
		      [SFunName,Counter]++
		      [NewType,FirstArg,SecondArg]),
    sourceForPlanFormulas(Formulas,
			  #parsing_struct{counter = Counter+1,
					  accum = Acc++SFormula,
					  info = SFunName}).


%% Replaces all elements in a predicate by its timestamp
%% Also deals with the action binary operations in a plan body
sourceForPredicate(#predicate{name = Name, arguments = Args,
			      annotations = Annotations}) ->  
    {sourceForTerm(Name), 
     list_to_tuple(sourceForTerms(tuple_to_list(Args))),
     sourceForTerms(Annotations)};
sourceForPredicate(BO = #binary_operation{
					 left_part = [Left],
					 right_part = [Right]}) ->
    BO#binary_operation{
      left_part = [sourceForTerm(Left)],
      right_part = [sourceForTerm(Right)]}.


sourceForTerms(Vars)->
    lists:map(fun jasonNewParser:sourceForTerm/1, Vars).



sourceForTerm(Pred) when is_record(Pred,predicate)->
    sourceForPredicate(Pred);
sourceForTerm(BO) when is_record(BO,binary_operation)->
    sourceForPredicate(BO);
sourceForTerm(#var{timestamp=TS}) -> % Bind is atom or integer
    TS;
sourceForTerm([]) ->
    [].

sourceForTestHandlingPlan()->
    "".





%% Generates the calls at the beginning of a plan trigger
%% that check if the parameters "pull the trigger"
source_param_matches(Params)->
    source_param_matches(Params,1,"").


source_param_matches([],_,Acc)->
    Acc;
source_param_matches([#var{timestamp=TS,
			       name=Name}|Rest],Counter,Acc) ->
    NewLine = 
	io_lib:format("\t   {Param~p, Bindings~p} ="++
		      "variables:try_param_match(Bindings~p,"++
		      "~p,Ejason_~s),\n",
		      [Counter,Counter,Counter-1,TS,Name]),
    
    source_param_matches(Rest,Counter+1,Acc++NewLine).


    

source_for_annotations(Annotations) ->
    Fun = fun (X) ->
		  sourceForPredicate(X)end,
    Preds =
	lists:map(Fun,Annotations),
    io_lib:format("~p",[Preds]).
		  





source_many_vars(Prefix,Num)->
    Vars = [ Prefix ++ integer_to_list(X) || X <- lists:seq(1,Num)],
    string:join(Vars,", ").



funForPredicate(#predicate{name = Name, arguments = Args},ArgsPrefix)->
    FunName = atom_to_list(Name#var.bind),
    Arguments = args_to_string(tuple_to_list(Args),ArgsPrefix),
    io_lib:format("~s(~s)",[FunName,Arguments]).


%% Turns a list of arguments into a string and
%% generates new variables for the predicates in the arguments
args_to_string(Vars,VarPrefix)->
 %   io:format("~n~n~nVars: ~p~n",[Vars]),
 %   io:format("VPrefix: ~p~n",[VarPrefix]),

    args_to_string(Vars,VarPrefix,
		   #parsing_struct{counter = 0,
				   info = {[],[],[]}}).


args_to_string([],_VarPrefix,PS =
	       #parsing_struct{info = {SAcc,NewArgs,NewVars}})->
    PS#parsing_struct{
      info = {string:join(lists:reverse(SAcc),","),
	      lists:reverse(NewArgs),
	      NewVars}};
args_to_string([Var|Vars], VarPrefix,
  PS = #parsing_struct{counter = Counter,
		       info = {SAcc,NewArgs,NewVars}})->  
   %io:format("Var: ~p~n",[Var]),
    NewPS = case Var of 
		#var{name=Name} ->
		  %  io:format("Name: ~p~n",[Name]),
		   % io:format("VPrefix: ~p~n",[VarPrefix]),
		    PS#parsing_struct{info = 
				      {[(VarPrefix++atom_to_list(Name))|SAcc],
					      [Var|NewArgs],NewVars}};
		#predicate{arguments = {}, name = PredName} ->
		    Name = PredName#var.name,
		    PS#parsing_struct{info = 
				      {[(VarPrefix++atom_to_list(Name))|SAcc],
				       [PredName|NewArgs],NewVars}};
		
		#predicate{} ->
		    VarName = "FUNPARAM"++integer_to_list(Counter),
		    PName = VarPrefix++VarName,
		    NewVar = #var{name= VarName,
			  bind  = utils:predicate_to_timestamp_tuple(Var)},
		   % io:format("NewVar: ~p~n",[NewVar]),
		    PS#parsing_struct{counter = Counter+1,
				      info = {[PName|SAcc],
					      [NewVar|NewArgs],[NewVar|NewVars]}}
	    end,
    %io:format("OUT\n"),
    args_to_string(Vars,VarPrefix,NewPS).
    
%    Triplets = 
%	lists:map(fun (X) ->
%			  jasonNewParser:arg_to_string(X,VarPrefix)
%		  end,Vars),

%    {StringVars,NewArgs,NewVars} = 
%	lists:unzip3(Triplets),
%
 %   {string:join(StringVars, ", "),
  %   lists:flatten(NewArgs),
   %  lists:flatten(NewVars)}.

   

%arg_to_string(Var = #var{name=Name},VarPrefix)->
%    {VarPrefix++atom_to_list(Name),Var,[]};
%% Predicates are represented by ONE variable in the arguments
%arg_to_string(Pred = #predicate{},VarPrefix) ->
%    PName = VarPrefix++"FUNPARAM",
%    NewVar = #var{name='FUNPARAM',
%	   bind  = utils:predicate_to_timestamp_tuple(Pred)},
%    {PName, NewVar,NewVar}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% ERLANG AGENTS
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generateAgentStart(Name)->
    Module = io_lib:format("-module(~p).~n~n",[Name]),
    Export = "-compile(export_all).\n\n",
    Import = "-include(\"macros.hrl\").\n"++
	"-include(\"ejason.hrl\").\n"++
	"-include(\"variables.hrl\").\n\n",
    NameMacro = io_lib:format("-define(Name,~p).~n",[Name]),
    InternalMacro = io_lib:format("-define(Internal,~p).~n",[Name]),
    EnvironmentMacro =io_lib:format("-define(Environment,~p).~n",[Name]),
    Start = 
	io_lib:format(
	  "start()->~n\tstart(1,~p,?NOTUNIQUE). \n\n"++
	  "start(Num) when is_number(Num)->\nstart(Num,~p,?NOTUNIQUE);\n\n"++
	  "start(Name) when is_atom(Name)->\nstart(1,Name,?UNIQUE).\n\n"++
	  "start(Num,Name,Uniqueness)->~n"
	  "\tAgName= utils:register_agent(Num,self(),Name,Uniqueness),~n"++
	  "\tAgent0 = reasoningCycle:"++
	  "start(AgName,[],[],belief_base:start()),~n", [Name,Name]),
    Init = io_lib:format("\tAgent1 = addInitialBeliefs(Agent0),\n"++
			 "\tAgent2 = addInitialGoals(Agent1),\n"++
			 "\tAgent3 = addPlans(Agent2),\n"++
		 "\treasoningCycle:reasoningCycle(Agent3#agentRationale"++
			 "{module_name=~p}).\n\n",[Name]),
			 io_lib:format("~s~s~s~s~s~s~s~s",
		  [Module,Export,Import,
		   NameMacro,InternalMacro,
		   EnvironmentMacro,Start,Init]).   


split_beliefs_rules(BelsAndRules)->
    split_beliefs_rules(BelsAndRules,[],[]).

split_beliefs_rules([],AccBels,AccRules)->
    {AccBels,AccRules};
split_beliefs_rules([Bel=#predicate{}|Rest],AccBels,AccRules) ->
    split_beliefs_rules(Rest,[Bel|AccBels],AccRules);
split_beliefs_rules([Rule=#rule{}|Rest],AccBels,AccRules) ->
    split_beliefs_rules(Rest,AccBels,[Rule|AccRules]).




parseAgents([])->
    ok;
parseAgents([Name|Names]) when is_atom(Name)->
    {ok,Tokens} = scanner:getTokens(io_lib:format("~p.asl",[Name])),
    {ok,[BelsAndRules,InitGoals,Plans]} = 
	jasonGrammar:parse(lists:flatten(Tokens)),
    {Beliefs,Rules} = split_beliefs_rules(BelsAndRules),
    SBeliefs = generateInitialBeliefs(Beliefs),
%      io:format("Rules: ~p~n",[Rules]),

    NewRules = group_rules(Rules),
 %   io:format("NewRules: ~p~n",[NewRules]),
    SRules = %string:join(lists:map(fun jasonNewParser:sourceForRule/1,NewRules),%			 " "),
	sourceForRules(NewRules),
    SInitGoals = generateInitialGoals(InitGoals),
    SPlans = generatePlans(Plans),
    %io:format("SPLANS: ~s~n",[SPlans]),
    AgentPart1 = generateAgentStart(Name),
    AgentPart2 = io_lib:format("~s~n~n~s~n~n~s~n~n~s~n~n",
			   [SBeliefs,SRules,SInitGoals,SPlans]),
    AgentPart3 = sourceForTestHandlingPlan(),
    SAgent = io_lib:format("~s~s~n~s",
			   [AgentPart1,AgentPart2,AgentPart3]),
  %  io:format("AGENT: ~n~s~n~n",[SAgent]),
    AgentFileName = io_lib:format("~p.erl",[Name]),
    {ok, AgentFile} = file:open(AgentFileName,[raw,write]),
    file:write(AgentFile,SAgent),
    file:close(AgentFile),
    parseAgents(Names).


% Groups rules with the same name and arity
group_rules(Rules)->
    group_rules(Rules,[]).

group_rules([],Groups) ->
    Groups;
group_rules([Rule=
	     #rule{head = #predicate{name=#var{bind = Name},
				    arguments = Args}}|Rest],Groups) ->
    Arity = size(Args),
    Key = {Name,Arity},
    NewGroups =  
	case lists:keytake(Key,1,Groups) of
	false ->
	    [{Key,[Rule]}|Groups];
	 {value,{Key,Value},OtherGroups}->
	    [{Key,[Rule|Value]}|OtherGroups]
	end,
    group_rules(Rest,NewGroups).

add_to_rule_group(Key,Rule,Groups) ->
    case lists:keytake(Key,1,Groups) of
	false ->
	    [{Key,[Rule]}|Groups];
	 {value,{Key,Value},NewGroups}->
	    [{Key,[Rule|Value]}|NewGroups]
    end.



%generatePlanRecords(PlanNameBindingsList)->
%    
%    SPlans = lists:map(fun generatePlan/1,PlanNameBindingsList) ++
%	[generate_test_goal_handler_plan()],

%    Source = string:join(SPlans,",\n"),
    
  %  io:format("SourceForPlans PLANS: ~s~n",[Source]),
%    io_lib:format("addPlans(Agent = #agentRationale{})->~n"++
%		  "Agent#agentRationale{plans = [~n~s]}.~n~n",
%		  [Source]).



%generatePlan({PlanName,Bindings})->
%    SBody = generatePlanRecordBody(PlanName,Bindings,1,""),
%                                        
%    io_lib:format(
%      "\t#plan{trigger=fun ?Name:~s_trigger/1 ,~n"++
%      "\tbody=~s,"++
%      "~n\tcontext=fun ?Name:~s_context/2}",[PlanName,SBody,PlanName]).



%generatePlanRecordBody(PlanName,[],_,Acc)->
%    io_lib:format(
%      "\t\t[~s,\nfun ~s_body_last_formula/1]", 
%      [string:join(lists:reverse(Acc),",\n\t\t"), PlanName]);
%generatePlanRecordBody(PlanName,[SingleBinding|Bindings],Num,Acc)->
%    Formula= io_lib:format("{fun ~s_body_formula_~p/2,~p}",
%			   [PlanName,Num,SingleBinding]),
%    generatePlanRecordBody(PlanName,Bindings,Num+1,[Formula|Acc]).
    
		

