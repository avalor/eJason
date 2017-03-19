next(X,Y) :-
   Y = X +1.

!start.

+!start <-
  .print("Let the class begin").
		   
+actual_count(Count)[source(Pupil)]: true <-
	.print("Pupil ",Pupil, " counts ", Count, " now.");
	-actual_count(Count)[source(Pupil)];
	?next(Count,Next);
	.send(Pupil,tell,actual_count(Next)).

