physical_limit(21).
beers_drunk(1).
inactive(robot).


!monitor(robot).

+!get(beer) : physical_limit(Limit) & 
	    beers_drunk(Drunk) & Drunk <= Limit & not inactive(robot)
   <-
   .send(robot, achieve, has(owner,beer));
   .print("Getting a beer"). 


+!get(beer):  physical_limit(Limit) & 
	    beers_drunk(Drunk) & Drunk >Limit & not inactive(robot)
   <-
     .print("I feel strangg..");
     .kill_agent(owner).


+has(owner,beer) : true 
   <-
   ?beers_drunk(Beers);
   .print("I got my beer number ", Beers, ". Yeepe!"); 
   +remaining_sips(3); 
   !!drink(beer).


+!drink(beer) : remaining_sips(Sips) & Sips > 0
   <-
     NewSips = Sips + -1;
     .print("Sip");     
     -+remaining_sips(NewSips);
     ?remaining_sips(X);
     !!drink(beer).


+!drink(beer) : remaining_sips(Sips) & Sips < 1
   <- 
   ?beers_drunk(Beers);
   NumBeers = Beers +1;
   -+beers_drunk(NumBeers);
   -has(owner,beer);
   .print("Finished beer!");
   !!get(beer).
 

+msg(M)[source(Ag)] : true 
   <- .print(Ag," says: ",M); 
      -msg(M);
      .print("Unacceptable!");
      .print("Let's restart my mechanic friend.");
      .kill_agent(Ag).


+closed(supermarket): true <-
    -closed(supermarket);
    !sleep.

+no_more(beer): true <-
    -no_more(beer);
    !sleep.

+!sleep: true <-
     .print("No beer means nap time. Zzz.");
     -closed(supermarket);
     .wait(2000);
     !!get(beer).

+agent_down(robot[container(Arch)]): true <-
      +inactive(robot);
      -agent_down(robot);
      .create_agent(robot,"robot.asl");
      .print("robot has stopped working. Start anew!");
      !monitor(robot).


+!monitor(robot):true <-
     -inactive(robot);
     .monitor(robot);
     !!get(beer).
