
init_count(0).
teacher_arch('arch2@avalor-laptop.fi.upm.es').
max_count(5).


		   
!startcount.

+!startcount : init_count(X) <- 
	    	+actual_count(X).
			
+actual_count(X) :  teacher_arch(Arch) &max_count(Y)& X < Y <-
		-actual_count(X);
		.send(teacher,Arch,tell,actual_count(X)).


+actual_count(X): max_count(Y)& X >= Y <-
            .print("Terminated count").
		

