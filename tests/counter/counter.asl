start.
init_count(0).
max_count(15).


next(X,Y) :-
    Y = X +1.

!startcount.


+!startcount : init_count(X) & max_count(Y) <- 
	     +actual_count(X);
	     !count.
			

+!count: actual_count(X)& max_count(Y)& X < Y <-
		?next(X, NewCount);
		.print("New Counter: ",NewCount);
		-+actual_count(NewCount);
		!count.



+!count: actual_count(X)& max_count(Y)& X >= Y <-
            .print("Terminated count").
		

