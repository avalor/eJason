/*
This agent belongs to the scenario test "testMonitor".
The agent killer:
     1) Monitors agent deader.
     2) Kills agent deader. Dead agent must be received (test3).
     3) Starts agent deader. Agent up must be received (test4).
     4) Kills agent deader again (test5). Connects to container home2@avalor...
     5) Created agent deader at home2. Agent up must be received (test6)
     6) Killing agent deader at home2. Agent down must be received (test7)
     7) Creates agent deader again. Disconnects from home2. Agent down must 
     	be received (test8).
     8) Connects to home2. Phase(4) starts.
     9) No agent up shall be received (test9).
    10) Monitors deader.Agent up must be rcvd (test10).
    11) End of the test. 
*/

!start.

+!start<-
   .print("I am alive");
   +phase(0);
   .monitor_agent(deader, persist([unknown_agent, other_stuff]));
	  // an agent_up is received, which disables the relation (Test3)
   .kill_agent(deader);
	// No notification shall be received
   .create_agent(deader, "deader.asl");
   .monitor_agent(deader, demonitor(unreachable_agent));
	  // an agent_up is received
   -+phase(1);
   .kill_agent(deader).
	// A notification shall be received


+agent_down(deader)[reason(unknown_agent)] <-
    .print("Deader not found");
    .send(testMonitor,tell, failed(deader_not_found_unknownagent)).


+agent_down(deader)[reason(dead_agent)]:phase(0) <-
    .print("Deader killed notification received. Failed test4");
    .send(testMonitor,tell, failed(deader_monitor_not_disabled_test4)).


+agent_down(deader)[reason(dead_agent)]:phase(1) <-
    .print("Deader killed (phase1)");
    .send(testMonitor,tell, passed(test3_prime));
    .create_agent(deader, "deader.asl"). // This will trigger plan A

+agent_down(deader)[reason(dead_agent)]:phase(2) <-
    .print("Deader killed (phase2)");
    .send(testMonitor,tell, passed(test5)).

+agent_down(deader)[reason(dead_agent)]: phase(3) <-
    .print("Deader killed (phase3)");
    .send(testMonitor,tell, passed(test7));
    .send(testMonitor,achieve,result(phase(2)));
    !start_phase3.


+agent_down(deader)[reason(dead_agent)]<-
    .print("Deader killed (NO PHASE!!!").

+agent_up(deader): phase(0) & not agent_up(deader)<-
    .print("Deader is alive. Monitoring disabled.");
    .send(testMonitor,tell, passed(test3)).

+agent_up(deader): phase(0) & agent_up(deader)<-
    .print("Deader is alive. Monitoring not disabled.").


// Plan A
+agent_up(deader): phase(1) <-
    .print("Deader is alive again!");
    .send(testMonitor,tell, passed(test4));
    .send(testMonitor,achieve, result(phase(1)));
    -+phase(2); //start with distribution
    .kill_agent(deader);      
    .wait(3000);
    .connect_to('home2@avalor-laptop.ls.fi.upm.es');
    .create_agent(deader[container('home2@avalor-laptop.ls.fi.upm.es')],
		  "deader.asl").

+agent_up(deader): phase(2) <-
    .print("Deader is alive again... in the newly connected container?");
    .container_of_agent(deader, ContName);
    .print("Deader runs in: ", ContName);
    ContName = 'home2@avalor-laptop.ls.fi.upm.es';
    .send(testMonitor,tell, passed(test6));
    .wait(1000);
    .print("Killing the agent far away");
    -+phase(3);
    .kill_agent(deader).

+agent_up(deader): phase(3) & not check(3)<-
    .print("Agent up (phase3)");
    .disconnect_from('home2@avalor-laptop.ls.fi.upm.es').

+agent_up(deader): phase(3) & check(3)<-
    .send(testMonitor, tell, failed(agent_up_received_test9));
    .kill_agent(killer).

// The monitor should have been disabled due to the disconnection
    
+agent_up(deader): phase(4) & not unreachable_received<-
    .print("Agent up (phase4 - test10)");
    +unreachable_received;
    //.disconnect_from('home2@avalor-laptop.ls.fi.upm.es');
    .send(testMonitor, tell, passed(test10)).


/*+agent_up(deader): phase(4) & unreachable_received<-
    .print("Agent up (phase4 - test12)");
    .send(testMonitor, tell, passed(test12));
    .send(testMonitor, achieve, result).*/



// Connection and disconnection
+!start_phase3<-
   .print("Starting phase 3");
    .create_agent(deader[container('home2@avalor-laptop.ls.fi.upm.es')],
		  "deader.asl");
    ??keep_on(phase3);
    .print("Execution resumed after wait test goal");
    -keep_on(phase3);
    +check(3);    
    .connect_to('home2@avalor-laptop.ls.fi.upm.es');
    .wait(500);
    .print("Starting phase 4");
    -+phase(4);
    -check(3);
    .send(testMonitor, tell, passed(test9));
    .print("Monitoring Deader (phase4)");
    -agent_up(deader);
    -agent_down(deader);
    .monitor_agent(deader, persist(any));
    .print("Monitored Deader (phase4)");
    .send(testMonitor, achieve, result).
    


+agent_down(deader)[reason(unreachable_agent)]:phase(3) <-
    .print("Deader is unreachable (test8)");
    .send(testMonitor, tell, passed(test8));
    +keep_on(phase3).	

    
+agent_down(deader)[reason(unreachable_agent)]:phase(4) <-
    .print("Deader is unreachable (test11)");
    .send(testMonitor, tell, passed(test11)).	



-agent_up(deader): phase(X) <-
      .print("No longer believe AGENTUP (phase",X,")").
  
-agent_down(deader): phase(X) <-
      .print("No longer believe AGENTDOWN (phase",X,")").
 


  


