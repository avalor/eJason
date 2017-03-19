init_count(0).
max_count(5).


!start.

+!start <-
  .connect_to('school@avalor-laptop.ls.fi.upm.es');
  !startcount.


+!startcount : init_count(X) <- 
	     .print("Counting from ",X);
	     +actual_count(X).

			
+actual_count(X) :  max_count(Y)& X < Y <-               
		-actual_count(X);
		.print("Next number is ",X);
		.send(teacher,tell,actual_count(X)).


+actual_count(X): max_count(Y)& X >= Y <-
            .print("Terminated count in ", Y).
		
