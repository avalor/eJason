next(X,Y) :-
   Y = X +1.


		   
+actual_count(Count)[source(Sender)]: true <-
	.print("Message from ",Sender);
	-actual_count(Count)[source(Sender)];
	?next(Count,Next);
	.send(Sender,tell,actual_count(Next)).

