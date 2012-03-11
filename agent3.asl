agradable("Hi").
agradable("How are you?").

inicio(1).
maximo(3).

persona(clara).
persona(lars_ake).
persona(alvaro).


inicio.

amable(Saludo,Persona):- 
     agradable(Saludo) & persona(Persona).


siguiente(X,Y):-
	maximo(NumeroMaximo) & Y = X +1  & NumeroMaximo >= Y.


!decir(Algo).


+!decir("How are you?"): true <-
	     .print("I am forced to say <How are you?>.\n I will say: \n\n");
	     !decir("Hi",X).

+!decir(X): amable(X,Y) <-
	    .print(X);
	    .print(Y).

+!decir(X,Y): amable(X,Y) <-
	    .print(X);
	    .print(Y).
