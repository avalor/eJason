// This agent:
//    1) Monitors agent deader. Unknown agent must be received (test1) 2
//    2) Creates agent deader. Agent up must be received (test2)
//    3) Creates agent killer.
//    4) Waits for results from phase1 (without distribution).
//    5) Waits for results form phase2 and phase3.


!start.

+!start <-
    .print("Started");
    .monitor_agent(deader, demonitor([created_agent,unreachable_agent])); 
                 // unknown_agent must be received (test1)
    
    .create_agent(deader,"deader.asl"). 
                 //created_agent must be received (test2)
    
+agent_down(deader)[reason(unknown_agent)] <-
    .print("Deader does not exist. (Test1 passed)");
    +passed(test1).

+agent_up(deader): not passed(test2) <-
//   -agent_up(deader); should not be necessary, as the RTS should do it
   .print("Deader is here. (Test2 passed)");
   +passed(test2);
   .create_agent(killer, "killer.asl").


+agent_up(deader): passed(test2) <-
    +failed(deader_monitor_not_disabled_agentup). 
                                         //the monitor to deader should be
                                         // disabled by created_agent

+agent_down(deader)[reason(dead_agent)] <-
    +failed(deader_monitor_not_disabled_deadagent). 
                                         //the monitor to deader should be
                                         // disabled by created_agent


+!result(phase(Y)): failed(X) <-
   .print("Failure due to reason \n", X).

+!result(phase(1)): (not failed(X)) & passed(test1) & passed(test2) & passed(test3) &   passed(test4) & not ok(phase1)<-
   +ok(phase1);
   .print("Success for phase1!!!").



+!result: passed(test1) & passed(test2) & passed(test3) & passed(test4) & 
          passed(test5) & passed(test6) & passed(test7) & passed(test8) &
 	  passed(test9) & passed(test10) &  not failed(X) <-
   .print("TOTAL SUCCESS!!!!").

+!result : failed(X) <-
   .print("FAILURE: reason ", X).



+passed(TestNum)[source(Source)] <-
  .print("Passed ",TestNum,".  Source: ",Source).
   

+failed(TestNum) <-
   C = [];
   D = [a,d];
  .print("Failed ",TestNum).



