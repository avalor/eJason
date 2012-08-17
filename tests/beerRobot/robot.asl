consumed(beer,0).
at(robot,owner).
limit(beer,10).
address(supermarket,'shopping_mall@avalor-laptop').


too_much(Beverage) :-
     limit(Beverage,Limit) &
     consumed(Beverage,Consumed) &
     Consumed > Limit.	



!monitor(supermarket).

+!has(owner,beer)
   :  not too_much(beer)
   <- 
      !at(robot,fridge);
      .send(fridge,achieve,give(beer)).


+!has(owner,beer)
   :  too_much(beer)     
   <- 
 
   ?limit(beer,Y);
   .print("The Department of Health does not allow me to give you more than ", 
   Y, " beers a day! I am very sorry about that!");
   ?consumed(beer,X);
   .print("Consumed ",X, " beers when the limit is ", Y, ".");
   .send(owner,tell,msg(" I am very sorry!")).    


+!has(owner, beer) 
   : closed(supermarket) <-
   .send(owner,tell,closed(supermarket));
   !monitor(supermarket).




+holding(beer) : true <-
    -holding(beer);
    !at(robot,owner);
    ?consumed(beer,X);
    Y = X +1;
    -+consumed(beer,Y);
    .send(owner, tell, has(owner,beer)).


+no_more(beer) 
   :  (not 
       closed(supermarket)) & address(supermarket,SupContainer)
   <- 
        -no_more(beer);
	.print("Fridge is empty.");
   	.send(supermarket, SupContainer,achieve, order(beer,5));
        .send(owner,tell,no_more(beer));
   	!at(robot,fridge).

+no_more(beer) 
   :  closed(supermarket)
   <- 
        -no_more(beer);
	.print("Fridge is empty. And supermarket is closed.");
       .send(owner,tell,closed(supermarket));
        !monitor(supermarket);
   	!at(robot,fridge).


   
+!at(robot,P) : at(robot,P) <- 
      .print("").


+!at(robot,P) : not at(robot,P)
  <- 
  -+at(robot,P).


+!monitor(Agent): address(Agent,Container) <-
      -closed(supermarket); 
      .print("Super");
      .monitor(Agent,Container).

+!monitor(owner): true <-
	.monitor(owner).


+agent_down(supermarket,Container)[reason(unknown_agent)]: true <-
    +closed(supermarket);			       
    .print("There is no supermarket").

+agent_down(supermarket,Container)[reason(isolated_agent)]: true <-
    +closed(supermarket);			       
    .print("I cannot find the supermarket").			       
    

+agent_down(supermarket,Container)[reason(dead_agent)]: true <-
    +closed(supermarket);			       
    .print("Supermarket is closed").			       
    

+agent_down(owner,Container): true <-
   .print("Oh, oh. My master has passed out.").

