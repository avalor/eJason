consumed(beer,0).
at(robot,owner).
limit(beer,3).
//address(supermarket,'shopping_mall@avalor-laptop.fi.upm.es').


too_much(Beverage) :-
     limit(Beverage,Limit) &
     consumed(Beverage,Consumed) &
     Consumed > Limit.	


!monitor(supermarket).
!monitor(owner).

+!has(owner,beer)
   :  not too_much(beer)
   <- 
      !monitor(owner);
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
   .my_name(MyName);
   .print("Killing ",MyName);
   .kill_agent(MyName).
//   .send(owner,tell,msg(" I am very sorry!")).    


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
       closed(supermarket)) //&address(supermarket,SupContainer)
   <- 
        -no_more(beer);
	.print("Fridge is empty.");
   	.send(supermarket,achieve, order(beer,5));
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


+!monitor(Agent): true <-//address(Agent,Container) <-
      -closed(supermarket); 
      .print(Agent," is now monitored.");//,Container);
      .monitor_agent(Agent).

+!monitor(owner): true <-
	.print("Monitoring the owner");
	.monitor_agent(owner).


+agent_down(supermarket)[reason(unknown_agent)]: true <-
    -agent_down(supermarket);				       
    +closed(supermarket);			       
    .print("There is no supermarket").

+agent_down(supermarket)[reason(unreachable_agent)]: true <-
    -agent_down(supermarket);				       
    +closed(supermarket);			       
    .print("I cannot find the supermarket").			       
    	


+agent_down(supermarket)[reason(dead_agent)]: true <-
     -agent_down(supermarket);				       
    +closed(supermarket);			       
    .print("Supermarket is closed").		       
    

+agent_down(owner): true <-
    -agent_down(owner);				    		  
   
   .print("Oh, oh. My master has passed out.").



