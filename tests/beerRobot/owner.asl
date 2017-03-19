physical_limit(10).
beers_drunk(1).
inactive(robot).

!start.

+!start <-
   .print("I feel like having a beer.");
   .monitor_agent(robot);
   !get(beer).


+!get(beer) : physical_limit(Limit) & 
	    beers_drunk(Drunk) & Drunk <= Limit & agent_up(robot)
   <-
   .send(robot, achieve, has(owner,beer));
   .print("Getting a beer"). 


+!get(beer):  physical_limit(Limit) & 
	    beers_drunk(Drunk) & Drunk >=Limit
   <-
     .print("I feel strangghh..");
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
     .wait(1000);     
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
 

+closed(supermarket): true <-
    -closed(supermarket);
    !sleep.

+no_more(beer): true <-
    -no_more(beer);
    !sleep.

+!sleep: true <-
     .print("No beer means nap time. Zzz.");
     -closed(supermarket);
     .wait(5000);
     !!get(beer).

+agent_up(robot): true <-
     !!get(beer).


+agent_down(robot): true <-
      +inactive(robot);
      .demonitor_agent(robot);
      -agent_down(robot);
      .print("Robot has stopped working. Start anew!");
      .create_agent(robot,"./robot.asl");
      .wait(1000);
      .monitor_agent(robot).

