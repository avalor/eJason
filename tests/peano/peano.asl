// Peano next (eq. decimal +1)
next(0,succ(0)).

next(succ(X), succ(Y)):-
	next(X,Y).


// Peano addition

sum(X,succ(Y),Z):-
    sum(succ(X),Y,Z).

sum(X,0,X):-
	true.


// Peano multiplication
mult(X,0,0):-
	true.

mult(0,Y,0):-
	true.

mult(succ(X),Y,Z):-
    mult(X,Y,Res1) & sum(Res1,Y,Z).


// Converts a peano number into its decimal equivalent
peano_to_dec(PeanoNum,DecNum):-
	peano_to_dec(PeanoNum,DecNum,0).

peano_to_dec(0,DecNum,DecNum):-
	true.

peano_to_dec(succ(PeanoNum),DecNum,Acc):-
	NewAcc = Acc + 1 &
	peano_to_dec(PeanoNum,DecNum,NewAcc).



// Converts a decimal number into its peano equivalent

dec_to_peano(DecNum, PeanoNum):-
         dec_to_peano(DecNum, PeanoNum, 0).

dec_to_peano(0, PeanoNum, PeanoNum):-
	true. 

dec_to_peano(DecNum, PeanoNum, Acc):-
	NewDecNum = DecNum - 1 &
	dec_to_peano(NewDecNum, PeanoNum, succ(Acc)).



// Initial goals and plan base
// !start(X,Y).	
!start(10, 40).


+!start (X, Y) : true <-
	.time(SH, SM,SS);
	.print("Start Time: ", SH,":",SM,":",SS);
     	?dec_to_peano(X, PeanoX);
	?dec_to_peano(Y,PeanoY);
	?sum(PeanoX,PeanoY, PSum);
	//?mult(PeanoX,PeanoY, PMult);
	?peano_to_dec(PSum, DSum);
	//?peano_to_dec(PMult, DMult);
	.print(X, " + ", Y," = ",DSum);
	//.print(X, " * ", Y," = ",DMult);
	.time(EH, EM, ES);	
	.print("End Time: ", EH,":",EM,":",ES).
	

+!printNum(VarName, PeanoNum) : 
	peano_to_dec(PeanoNum,DecNum) <-
	.print(VarName, " = ", DecNum).