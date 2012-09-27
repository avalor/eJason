next(0,succ(0)).

next(succ(X),succ(Y)) :-
	next(X,Y).


sum(X,succ(Y),Z):-
    sum(succ(X),Y,Z).

sum(X,0,X):-
	true.


mult(X,0,0):-
	true.

mult(0,Y,0):-
	true.


mult(succ(X),Y,Z):-
    mult(X,Y,Res1) & sum(Res1,Y,Z).

peano_to_dec(PeanoDec,DecNum):-
	peano_to_dec(PeanoDec,DecNum,0).

peano_to_dec(0,DecNum,DecNum):-
	true.

peano_to_dec(succ(PeanoDec),DecNum,Acc):-
	NewAcc = Acc + 1 &
	peano_to_dec(PeanoDec,DecNum,NewAcc).
   

!start.


+!start : true <- 
	?sum(succ(succ(0)),succ(succ(succ(0))), Z);
	.print("2 + 3 = ",Z);
	!printNum(Z,Z);
	.my_name(Name);
	.print(Name);
	.my_container(Arch);
	.print(Arch);
        ?mult(Z,succ(succ(succ(0))), W);
	.print("5 * 3 = ",W);
	!printNum(W,W).



+!printNum(VarName, PeanoNum) : 
	peano_to_dec(PeanoNum,DecNum) <-
	.print(VarName, " = ", DecNum).