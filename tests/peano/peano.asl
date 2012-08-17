next(0,succ(0)).


next(succ(X),succ(Y)) :-
	next(X,Y).


sum(X,succ(Y),Z):-
    sum(succ(X),Y,Z).


sum(X,0,Z) :-
   Z = X.

mult(X,0,Z):-
	Z = 0.

mult(0,Y,Z):-
	Z = 0.


mult(succ(X),Y,Z):-
    mult(X,Y,Res1) & sum(Res1,Y,Z).

   

!start.


+!start : true <- 
	?sum(succ(succ(0)),succ(succ(succ(0))), Z);
	.print(Z);
	.my_name(Name);
	.print(Name);
	.my_architecture(Arch);
	.print(Arch);
        ?mult(Z,succ(succ(succ(0))), W);
	.print(W).
	
