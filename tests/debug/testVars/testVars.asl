// Test suite for the different possible variable unification alternatives
// Corner cases are searched for


regla(Parametro1) :-
   true.

regla(Parametro1,Parametro2):-
   true.


//Rule that implements classical map

map(_, [], []):-
       true.

map(Rule, [Value|Rest], [RuleResult|Accum]):-
      Rule[value(Value),result(RuleResult)] &
      map(Rule,Rest,Accum).


//Rule that implements classical foldl

foldl(_,[],Result, Result):-
      // .print("FOLDL TOTAL RESULT: ", Result) &
      // .wait(500) &
        true.


foldl(Rule, [Value|Rest], Accum, Result):-
    //.print("FOLDL List: ", [Value|Rest]) &
    Rule[values(Value,Accum),result(NewAccum)] &
     //.print("FOLDL NEWACCUM: ", NewAccum) &
     // .wait(500) &
     foldl(Rule,Rest,NewAccum,Result).


//Rule that implements classical foldr

foldr(Rule,List,InitValue,Result):-
    //.print("Reversing: ",List)&
    //.wait(100)&
    reverse(List,ReverseList) &
    foldl(Rule,ReverseList,InitValue,Result).



//Rule that implements classical reverse
reverse([],[]):-
  true.

reverse(List, Result) :-
  //.print("Elem1: ",Elem) &
  //.wait(200) &
  reverse(List, [],Result).


reverse([],Result,Result):-
 //.print("Result: ",Result) &
  //.wait(200) &
  true.


reverse([Elem|Rest], Accum, Result):-
      //  .print("Elem2: ",Elem) &
       // .print("Rest2: ",Rest) &
        //.print("Accum: ",Accum) &
        //.print("Compose: ",[Elem|Accum]) &

	  //.wait(500) &

	reverse(Rest,[Elem|Accum],Result).




// Rule that sums two values
 sum_rule(Sum)[value(Value),result(Result)]:-
    Result = Sum + Value.


sum_rule[values(Value1,Value2),result(Result)]:-
   Result = Value1 + Value2.


// Unifies Res with all integers in [A,B]

sequence(A,B,A):-
   //.print("Trying with ", B, ">=", A) &
    B >= A.

sequence(A,B,C):-
    B >= A &
    D = A +1 &    
   //.print("Trying with ", D) &
    sequence(D,B,C).



!test0(a,[1,2,3]).

////////////Testing lists, true/false in context and Var[] 

+!test0(A,[B|C]): true & not not (not not true | not false) <-
  .print("Starting test0");
   A[] = a[no];
  .print("A is a: A = ",A);
   1 = B[ignore,C];
  .print("B is 1: B = ",B);
  .print("C is [2,3]: C = ",C);
  .print("Finishing test0\n\n\n");
  !test1([C|D],~a).



//////////Testing rel_gt, rel_lt, log_not, ?UNBOUNDLIST and ?STRONGNEG

+!test1([[A,B]|List], D): A < B & B > A & not B < A &  List = 3<-
    .print("Failed test1 as List cannot be 3 (must be list). List: ",List).

+!test1([[A,B]|List], D): A < B & B > A & not B < A & List = [3]<-
   .print("Starting test1");	    
    //??wait(Infinite);
    2 = A[c,d,G[D]];
{{{{   .print("A is 2: A = ",A);
   .print("C is unbound: C = ",C);
   .print("D is ~a: D = ",D);
   .print("List is [3]: List = ",List);
    Neg = D[label(A)];
   .print("Neg is ~a[label(2)]: Neg = ",Neg);
   .print("Finishing test1\n\n\n");
  !test2(List,Neg)[lista([1,2,3]),string("ahora","luego")[label(Free)]]}}}}.



 //////////Testing annotations in plans and context

+!test2([A,Empty],_) <-
  .print("Failed test2 as [A,Empty] cannot match [3]").

+!test2([A|Empty],a) <-
  .print("Failed test2 as a cannot match ~a").

+!test2([A|Empty],_)[string(C)] <-
  .print("Failed test2 as string(C) cannot match string(C,D)").

+!test2([A|Empty],_)[string(C,D)[c]] <-
  .print("Failed test2 as string(C,D)[c] cannot match string(A,B)[label]").


+!test2([A|Empty],_)[string(C,D)[label]]:  .print("hola1") <-
  .print(A, Empty, C,D);
  .print("Failed test2 as string(C,D)[label] cannot match string(C,D)[label(F)]").

+!test2([A|Empty],_)[string(C,D)[label(E)], lista(F,G,A)] <-
  .print("Failed test2 as lista([1,2,3]) cannot match lista(1,2,3)").


+!test2([A|Empty],_)[string(C,D)[label(E)], lista([F,G,A])] :
    .print("Starting test2") & not not .print("Empty is: ",Empty) <- 
    NextPlan = test3[a_label];
    .print("String: ",a("string"));
    .print("Finishing test2\n\n\n");
    !NextPlan.


////////// Testing the use of vars in add_achievement_goal

+!test3[Label] <-
  .print("Starting test3"); 
  .print("Label is a_label. Label: ",Label);  
  NextPlan = sub_test3_1[matched_label(FreeVar)];
  .print("NextPlan: ",NextPlan);
  !NextPlan[FreeVar[algo]];
  .print("Freevar must be match. FreeVar: ",FreeVar);
  !sub_test3_2(Num,Num[var]);
  .print("Num is 1. Num: ",Num);  
  .print("Finishing test3\n\n\n");
  !test4.

+!sub_test3_1[matched_label(match),match] <-
    .print("Label matched properly").


+!sub_test3_2(1,1)<-
   true.


////////// Testing the addition/deletion of beliefs (without free variables)
+!test4 <-
  .print("Starting test4"); 
  A = 1;
  +belief(A);
  +belief(2);
   B = B[2];
  .print("B should be unbound. B: ",B);
  -belief(B);
  +new_belief(1,3)[arg1,arg(2,1)];
   Free = Free;
  .print("Free should be unbound. Free: ",Free);
  .print("E should be unbound. E: ",E);
  .print("Finishing test4\n\n\n");
  !!test5(UnboundVar).




+!test5(A): A[3] = A <-
 .print("Fail test5 because of A: ",A).

+!test5(A): A = [1,3,A] <-
 .print("Fail test5 because of A: ",A).

+!test5(A): pred(1,2,A) = A <-
 .print("Fail test5 because of A: ",A).

+!test5(A): A = A[3] & new_belief(A,B)[arg(C,2)]<-
   .print("Fail test5 because of 2 =/=1\n").

+!test5(A): A = A[3] & new_belief(A,B)[arg(C,Z),Z]<-
   .print("Fail test5 because of Z. Z: ",Z,"\n").

+!test5(A): A = A[3] & new_belief(A,B)[arg(C,Z),source(Source)]<-
  .print("Starting test5");
  .print("Source should be self. Source: ",Source);
  .print("Z should be 1. Z: ",Z);
{{  Num = Z +3 + (4+5) * 6 + 7 * -(2**3 - 3) div (3-1) mod 5; 
  .print("Num should be 56. Num: ",Num);
  Neg = ~list([1,2,Num]);
  .print("Neg should be ~list([1,2,56]). Neg is: ",Neg);
  +Neg}};
  +some_list([1,2,3,4,5,6,7,8])[says(pepe)];
  ?some_list([First|Rest])[source(Source),says(Pepe)];
 .print("First should be 1. First: ",First);
 .print("Rest should be [2,3,4,5,6,7,8]. Rest: ",Rest);
 .print("Finishing test5\n\n\n");
  !test6.



// Testing rules
+!test6: some_list(A,B) <-
    .print("Fail test6 because of B. B: ",B,"\n").

+!test6: some_list(List) & foldl(sum_rule(1),List,0,Result) & Result \== 37 <-
     .print("Fail test6 because of Result. Result: ",Result,"\n").


+!test6: some_list(List) & foldl(sum_rule(1),List,0,Result1) & 
        foldr(sum_rule(1), List,0,Result2) & Result1 = Result2 +1 <-
     .print("Fail test6 because of Result1. Result1: ",Result1,"\n").

+!test6: some_list(List) & foldl(sum_rule(1),List,0,Result1) & 
        foldr(sum_rule(1), List,0,Result2) & Result1 == Result2 &
        Result1 = Result2 & list(NegList) <-
     .print("Fail test6 because of NegList. NegList: ",NegList,"\n").

	
+!test6: some_list(List) & foldl(sum_rule,List,0,Result1)  &
        foldr(sum_rule, List,0,Result2) & .print("Result1: ", Result1) &
	.print("Result2: ", Result2) &
	Result1 == Result2 & Result1 = Result2 & 
	List2 = list([A|B]) &  ~List2  <-	
  .print("Starting test6");
   NegList = ~List2;
   TryList = [1,2,3];
  .print("NegList must be ~list([1,2,56]). NegList: ", NegList);
  .print("B must be [2,56]. B: ", B);
    .print("Finishing test6\n\n\n");
   !test7(NegList).



// Testing the backtracking of rules

+!test7(~A): sequence(1,10, Val) & sequence(11,20,Val)<-
     .print("Fail test7 because of Val not in [1,10] and [11,20]. Val: ",Val).

+!test7(~A): sequence(1,10, Val) & sequence(8,-20,Val)<-
     .print("Fail test7 because of Val not in [1,10] and [8,-20]. Val: ",Val).

+!test7(~A): sequence(1,100, Val) & sequence(-10,1,Val)<-
     .print("Starting test7");
     .print("A must be list([1,2,56]). A: ", A);
     ?~NegBelief;
     .print("NegBelief must be list([1,2,56]). NegBelief ", NegBelief);
     .print("Val must be 1. Val: ", Val);
    .print("Finishing test7\n\n\n");
    .print("Success!").




+Belief <-
  .print("Added Belief: ",Belief).

-Belief <-
   .print("Removed Belief: ",Belief).

-!Goal <-
  .print("Failed achievement goal: ",Goal).

-?Goal <-
  .print("Failed test goal: ",Goal).