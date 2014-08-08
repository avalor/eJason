all:
	rm -f ebin/actions.beam ebin/belief_base.beam ebin/conditions.beam \
		ebin/ejason_distribution_manager.beam \
		ebin/ejason_supervision_manager.beam \
		ebin/ejason.beam ebin/iterator.beam ebin/jasonGrammar.beam \
		ebin/jasonNewParser.beam \
		ebin/operations.beam ebin/reasoningCycle.beam \
		ebin/scanner.beam ebin/test_goal.beam ebin/utils.beam \
		ebin/variables.beam 

	erlc -W0 -o ebin ./src/actions.erl ./src/belief_base.erl	\
		./src/conditions.erl					\
		./src/ejason_distribution_manager.erl			\
		./src/ejason_supervision_manager.erl ./src/ejason.erl	\
		./src/iterator.erl ./src/jasonGrammar.erl		\
		./src/jasonNewParser.erl ./src/operations.erl		\
		./src/reasoningCycle.erl ./src/scanner.erl		\
		./src/test_goal.erl ./src/utils.erl			\
		./src/variables.erl