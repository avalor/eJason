init_count(0).
max_count(5).


!startcount.


+!startcount : init_count(X) <- 
	    	+actual_count(X).

			
+actual_count(X) :  max_count(Y)& X < Y <-               
		-actual_count(X);
		.send(teacher,tell,actual_count(X)).


+actual_count(X): max_count(Y)& X >= Y <-
            .print("Terminated count").
		
