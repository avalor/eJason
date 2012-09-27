contents(beer,4).


+!give(Item)[source(Who[container(Container)])]:
    contents(Item,Stock) & Stock > 0 <-
    .print("Giving beer to ",Who, " in ", Container);
    NewStock =  Stock + -1;
    -+contents(Item,NewStock); 
    .print("Beers left:  ", NewStock);
    .send(Who,tell,holding(Item)).


+!give(Item)[source(Who)] :
    contents(Item,Stock) & Stock < 1 <-
    .send(Who,tell,no_more(Item)).



+delivered(Item,Qtd, OrderId)
  :  contents(Item,Stock)
  <- 
     -delivered(Item,Qtd,OrderId);
     .print("Received ", Qtd, " units of ", Item);
     NewStock = Qtd + Stock;
     -+contents(Item,NewStock). 

