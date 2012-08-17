next(X,Y) :-
  	       Y = X +1.

		   
+actual_count(Count)[source(Pupil,Arch)]:true <-
	-actual_count(Count)[source(Pupil,Arch)];
	?next(Count,Next);
	.send(Pupil,Arch,tell,actual_count(Next)).
