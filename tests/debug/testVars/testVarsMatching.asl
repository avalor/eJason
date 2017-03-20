// This example shows how the different variables in several
// parts of the plan are correctly matched

// Should end with "My C is 2 and my X is 12"


hola_rule(A):-
	A = 23.

!start(1,D).

+!start(1,2) : true <-
   .print("hola");
   !hola(1,2,C,D)[L];
   .print("L = ", L);
   .print("Out of hola");
   ?error(X)[anotacion];
   .print("My C is ", C," and my X is ",X).

+!hola(A,B,C,D)[hallo(adeu)[holanda]]: C = 2<-
    D = 3;
    .print("D=3? -> D = ",D);
    +hello;
    ?hello.

-!hola(A,B,C,D)[Z]: true <-
   .print("A=",A," B=",B," C=",C, " Z=",Z," D=",D).

-?error(Z): Z = 12 <-
	    .print("Into -?error(Z)");
	    ?hola_rule(B)[Tu];
	    .print("B =",B, " My: ",Tu, " Annot: ",Annot);
	    .print("Z is ", Z).
