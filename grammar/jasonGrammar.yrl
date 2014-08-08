Nonterminals agent init_bels beliefs init_goals plans plan
	     triggering_event literal context log_expr simple_log_expr
	     not_expr body body_formula atomic_formula list_of_terms
	     term list rel_expr rel_term arithm_expr arithm_term
	     plan_label plan_context plan_body body_formulas
	     arithm_symbol arithm_terms body_symbol trigger_symbol
	     list_tail right_rel_expr terms rel_symbol
	     crit_sect_body_formulas 
	     list_of_terms_brackets body_external_action.

Terminals atom var var_label string number neg_number '(' ')' dot  rel_string
	  '~' '+' '-' '!' '?' '??' '@' '[' ']' '|' '*' '**' '{{' '}}'
	  'div' 'mod' '/' ',' '<' '>' '<=' '>=' '=' '==' '=..' 
	  ':-' ':' ';' '<-' '&' '!!' '-+' '\\==' '.' not.
Rootsymbol agent.
agent -> init_bels init_goals plans:    
      ['$1', '$2','$3'].

init_bels ->beliefs:
	  '$1'. 	    	     

beliefs -> literal dot beliefs:
	Belief = jasonNewParser:make_belief('$1'),
	[Belief|'$3'].
beliefs -> literal ':-' log_expr dot beliefs:
      Res = jasonNewParser:make_rule({'$1', eJasonRule,['$3']}),
      [Res|'$5'].
beliefs -> '$empty':[].

init_goals -> '$empty':[].
init_goals -> '!' literal dot init_goals:
	   Goal = jasonNewParser:make_event(add_achievement_goal,'$2'),
	   [Goal|'$4'].

plans -> '$empty':[].
plans -> plan plans:['$1'|'$2'].

plan -> plan_label triggering_event plan_context plan_body:
     jasonNewParser:make_plan('$2','$3','$4').

plan_label -> '@' atomic_formula.
plan_label -> '$empty'.

plan_context -> ':' context:'$2'.
plan_context -> '$empty':
	     %% An empty context amounts to writing "true"
	     {atom,0,true}. 

plan_body -> '<-' body:'$2'.
plan_body -> '$empty':[].

triggering_event -> '+' trigger_symbol literal:
		 case '$2' of
		      achievement_goal ->
		      		   {add_achievement_goal,'$3'};
		      test_goal ->
		      		   {add_test_goal,'$3'};
		      belief ->
				   {add_belief,'$3'}
                 end.

triggering_event -> '-' trigger_symbol literal:
		 case '$2' of
		      achievement_goal ->
		      		   {failed_achievement_goal,'$3'};
		      test_goal ->
		      		   {failed_test_goal,'$3'};
		      belief ->
				   {remove_belief,'$3'}
                 end.	

triggering_event -> '+' trigger_symbol var:
		 case '$2' of
		      achievement_goal ->
		      		   {add_achievement_goal,'$3'};
		      test_goal ->
		      		   {add_test_goal,'$3'};
		      belief ->
				   {add_belief,'$3'}
                 end.

triggering_event -> '-' trigger_symbol var:
		 case '$2' of
		      achievement_goal ->
		      		   {failed_achievement_goal,'$3'};
		      test_goal ->
		      		   {failed_test_goal,'$3'};
		      belief ->
				   {remove_belief,'$3'}
                 end.



	   
trigger_symbol -> '!':achievement_goal.
trigger_symbol -> '?':test_goal.
trigger_symbol -> '$empty':belief.

literal -> '~' var: {strongNeg,'$2'}.
literal -> '~' atomic_formula:{strongNeg,'$2'}.
literal -> atomic_formula: 
	
	%%io:format("ATOMIC FORMULA: ~p~n",['$1']),
	'$1'.

context -> log_expr:
	%%io:format("~n~n~n~nContext is: ~p~n",['$1']),
	'$1'.


%% log_expr -> true:
%% 	 true.

%% log_expr -> false:
%% 	 %%io:format("RETURNINGFALSE~n"),
%% 	 false.


log_expr -> simple_log_expr:'$1'.


log_expr -> not_expr:
	 '$1'.


%% Must be simple_log_expr, otherwise 

not_expr -> not not_expr:
	 Condition = '$2',
	 %% case '$2' of 
	 %%    {formula,false,[],_} ->
	 %%          {atom,0, false};
         %%    _ ->
	 %%    	  '$2'
         %%  end,
	 {log_not, Condition}.

not_expr -> not simple_log_expr:
	 Condition = '$2',
	 {log_not, Condition}.


not_expr -> not '(' log_expr ')':
  	 {log_not,'$3'}.%%[] added to match the parsing function


log_expr ->  log_expr '&' log_expr:
	 %% io:format("LogAnd 1: ~p~nLogAnd2: ~p~n",
	 %% ['$1','$3']),
	 {log_and,'$1','$3'}.

log_expr ->  log_expr '|' log_expr:
	 {log_or,'$1','$3'}.

log_expr -> '(' log_expr ')':
	 %% Here it ignores the parenthesis for the whole context.
	 %% TODO: check if it does not delete the ones inbetween
	 {parenthesis,'$2'}.



simple_log_expr -> atom '.'  body_external_action: 
		P = {package, '$1'},
		Res = execute(parse_internal_action,[P,'$3']),
		%% io:format("InternalAction in context or rule: ~p~n",
                %%            [Res]),
		Res.

simple_log_expr -> '.'  body_external_action: 
		P = {no_package},
		Res = execute(parse_internal_action,[P,'$2']),
		%% io:format("InternalAction in context or rule: ~p~n",
                %%            [Res]),
		Res.

simple_log_expr -> literal:'$1'.
simple_log_expr -> rel_expr:'$1'.



body -> body_formulas:'$1'.

body_formulas -> body_formula ';' body_formulas:['$1'|'$3'].
body_formulas -> '{{' crit_sect_body_formulas '}}' ';' body_formulas:
	      [{critical_section,'$2'}|'$5'].
body_formulas -> body_formula dot:['$1'].
body_formulas -> '{{' crit_sect_body_formulas '}}' dot : '$2'.

body_formula -> body_symbol var:{action,'$1','$2'}.
body_formula -> body_symbol literal:{action,'$1','$2'}.
body_formula -> body_external_action:'$1'.
body_formula -> rel_expr:{action,'fun' ,'$1'}.

crit_sect_body_formulas -> body_formula ';' crit_sect_body_formulas:
       ['$1'|'$3'].

crit_sect_body_formulas -> '{{' crit_sect_body_formulas '}}' ';' crit_sect_body_formulas:
       ['$1'|'$3'].

crit_sect_body_formulas -> body_formula :
    ['$1'].
crit_sect_body_formulas -> '{{' crit_sect_body_formulas '}}': '$2'.






%% OLD down

%% body -> body_formula body_formulas dot:['$1'|'$2'].

%% body_formulas -> ';' body_formula body_formulas:['$2'|'$3'].
%% body_formulas -> '$empty':[].


%% body_formula -> body_symbol var:{action,'$1','$2'}.
%% body_formula -> body_symbol literal:{action,'$1','$2'}.
%% body_formula -> body_external_action:'$1'.
%% body_formula -> rel_expr:{action,'fun' ,'$1'}.

%% body_formula -> '{{' body '}}': {critical_section,'$2'}.



body_symbol -> '!':add_achievement_goal.
body_symbol -> '!!':new_intention_goal.
body_symbol -> '?':add_test_goal.
body_symbol -> '??':add_no_wait_test_goal.
body_symbol -> '+':add_belief.
body_symbol -> '-':remove_belief.
body_symbol -> '-+':remove_add_belief.

body_external_action -> atomic_formula:{action,external_action,'$1'}.
body_external_action -> atom '.' body_external_action:
		     P = {package, '$1'},
		     execute(parse_internal_action,[P,'$3']).		     
body_external_action -> '.' body_external_action:
       execute(parse_internal_action,[{no_package},'$2']).


atomic_formula -> atom list_of_terms list_of_terms_brackets:
	      % io:format("atom Formula: ~p~n",['$1']),
	      % io:format("Annotations: ~p~n",['$3']),
	      % io:format("Terms: ~p~n",['$2']),
	       {atom,_,Atom} = '$1',
	       {formula,Atom,'$2',
		'$3'}.
	      % jasonNewParser:parse_vars_in_annotation('$3')}.

atomic_formula -> var_label list_of_terms list_of_terms_brackets:
	        %% io:format("var_label: ~p~n",['$1']),
	        %% io:format("Annotations: ~p~n",['$3']),
		%% timer:sleep(3000),
	       {var_label,Line,Variable} = '$1',
	       {formula,{var,Line,Variable},'$2',
       	      '$3'}.
	      % jasonNewParser:parse_vars_in_annotation('$3')}.


%% atomic_formula -> var_fun list_of_terms list_of_terms_brackets:
%% 	       io:format("var_fun: ~p~n",['$1']),
%% 	       io:format("Annotations: ~p~n",['$3']),
%% 	       {var_fun,Line,Variable} = '$1',
%% 	       {formula,{var,Line,Variable},'$2',
%% 	       jasonNewParser:parse_vars_in_annotation('$3')}.



list_of_terms -> '(' term terms ')':
	     % io:format("TERM: ~p~n",['$2']),
	      ['$2'|'$3'].
list_of_terms -> '$empty':[].

terms -> ',' term terms:['$2'|'$3'].
terms -> '$empty':[].


list_of_terms_brackets -> '$empty':[].
list_of_terms_brackets -> '[' term terms ']':['$2'|'$3'].
list_of_terms_brackets -> '['']':[].


%term -> list:'$1'.
term -> rel_term:'$1'.
term -> string:'$1'.

list -> '[' ']':{list,[],'[]'}.
list -> '[' term terms list_tail ']':{list,['$2'|'$3'],'$4'}.

list_tail -> '|' list:'$2'.
list_tail -> '|' var:'$2'.
list_tail -> '$empty':'[]'.



rel_expr -> rel_term rel_symbol rel_term right_rel_expr:
	 LSide = {'$2','$1','$3'},
	 RSide = '$4',
	 case RSide of
	      [] ->
	      	 LSide;
	      {RelSymbol,RightTerm} ->
	         {RelSymbol,LSide,RightTerm}
         end.


right_rel_expr -> rel_symbol rel_term right_rel_expr:
	       case '$3' of
	       	    []->
		       {'$1','$2'};
		    {RelSymbol,RightTerm} ->
	               {'$1',{RelSymbol,'$2',RightTerm}}
               end.
right_rel_expr -> '$empty':[].

rel_symbol -> '<':rel_lt.
rel_symbol -> '<=':rel_le.
rel_symbol -> '>':rel_gt.
rel_symbol -> '>=':rel_ge.
rel_symbol -> '==':rel_eq.
rel_symbol ->'\\==':rel_diff.
rel_symbol ->'=':rel_assig.
rel_symbol ->'=..':rel_decomp.

rel_term -> literal:
	 '$1'.
rel_term -> arithm_expr:
	PrefixNotation = execute(to_polish_notation,['$1']),
      %% io:format("Arithmetic Expression: ~p~n",[PrefixNotation]),
      PrefixNotation.
rel_term -> list:
	 '$1'.
rel_term -> rel_string:
	 erlang:setelement(1,'$1',string).


arithm_expr -> arithm_term  arithm_terms:
	% io:format("~p y ~p~n",['$1','$2']),

	    case '$2' of
	      	 [] ->
		    '$1';
		 {ArithSymbol, ArithExpr}->
		     {ArithSymbol,'$1',ArithExpr}
            end.

arithm_terms ->  neg_number arithm_terms:
	     {neg_number,Line, Number} = '$1',
 	      
	    case '$2' of
	      	 [] ->
		    {arith_minus,{number,Line,-Number}};
		 {ArithSymbol, ArithExpr}->
		     {arith_minus,{ArithSymbol,{number,Line,-Number},ArithExpr}}
            end.


arithm_terms -> arithm_symbol arithm_term arithm_terms:
	    case '$3' of
	      	 [] ->
		    {'$1','$2'};
		 {ArithSymbol, ArithExpr}->
		     {'$1',{ArithSymbol,'$2',ArithExpr}}
            end.

arithm_terms -> '$empty':[].

arithm_symbol -> '+':arith_plus.
arithm_symbol -> '-':arith_minus.
arithm_symbol -> '*':arith_mult.
arithm_symbol -> '**':arith_power.
arithm_symbol -> '/':arith_slash.
arithm_symbol -> 'div':arith_div.
arithm_symbol -> 'mod':arith_mod.
%% arithm_symbol -> $empty: 
%% 	      {neg_number,_Line, NegNum} = '$1',
%%	      .

arithm_term -> number:'$1'.

arithm_term -> neg_number:
	      {neg_number,Line, NegNum} = '$1',
	      {number,Line, NegNum}.

arithm_term -> '-' arithm_term:
	    {opposite, '$2'}.
arithm_term -> var:'$1'.
arithm_term -> '(' arithm_expr ')':
	    %%io:format("Term is: ~p~n",['$2']),
	    {parenthesis,'$2'}.




Erlang code.

execute(FunName,Args)->
	apply(jasonNewParser,FunName,Args).
