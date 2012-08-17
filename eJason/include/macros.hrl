
-define(BINARYOPERATORS,
	[rel_lt,rel_gt,rel_le,rel_ge,rel_eq,rel_diff,
	 rel_assig,rel_decomp,arith_plus,arith_minus,
	 arith_mult,arith_slash,arith_div,arith_mod,
	 log_and,log_or]).

-define(ATOMTERMS,
	[atom,string,number]).

-define(BANGSENTENCE,[add_belief,add_achievement_goal]).

-define(ADDBELIEF,add_belief).

-define(REMOVEBELIEF,remove_belief).

-define(REMOVEADDBELIEF,remove_add_belief).

-define(ADDACHGOAL,add_achievement_goal).

-define(ADDINTENTIONGOAL,new_intention_goal).

-define(ADDTESTGOAL,add_test_goal).

-define(PLANFAILED,plan_failed).

-define(UNIQUE,true).

-define(NOTUNIQUE,false).
