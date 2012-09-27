init_count(0).
teacher_address(teacher[container('arch2@babel.ls.fi.upm.es')]).

max_count(5).


!startcount.


+!startcount : init_count(X) <- 
	    	+actual_count(X).

			
+actual_count(X) :  teacher_address(Arch) &max_count(Y)& X < Y <-               
		-actual_count(X);
		.send(Arch,tell,actual_count(X)).


+actual_count(X): max_count(Y)& X >= Y <-
            .print("Terminated count").
		
