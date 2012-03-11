init_count(0).
max_count(2000).

siguiente(X,Y) :-
  	       Y = X +1.

!startcount.

+!startcount : init_count(X) <- +actual_count(X);
			!count.
			
+!count: actual_count(X)& max_count(Y)& X < Y <-
		?siguiente(X, NewCount);
		-+actual_count(NewCount);
		!count.


+!count: actual_count(X)& max_count(Y)& X >= Y <-
            .print("Terminated count").
		

