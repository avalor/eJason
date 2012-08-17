contents(beer,4).


+!give(Item)[source(Who,Where)] :
    contents(Item,Stock) & Stock > 0 <-
    .print("Giving beer to ",Who, " in ", Where);
    NewStock =  Stock + -1;
    -+contents(Item,NewStock); 
    .print("Beers left:  ", NewStock);

    .send(Who,Where,tell,holding(Item)).


+!give(Item)[source(Who,Where)] :
    contents(Item,Stock) & Stock < 1 <-
    .send(Who,Where,tell,no_more(Item)).



+delivered(Item,Qtd, OrderId)
  :  contents(Item,Stock)
  <- 
     -delivered(Item,Qtd,OrderId);
     .print("Received ", Qtd, " units of ", Item);
     NewStock = Qtd + Stock;
     -+contents(Item,NewStock). 

