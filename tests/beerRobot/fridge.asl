contents(beer,4).

//!give(beer)[source(pepe[container(home)])].

!connect.

+!connect: true <-
   .connect('shopping_mall@avalor-laptop.fi.upm.es').

+!give(Item)[source(Who)]:
    contents(Item,Stock) & Stock > 0 <-   
    .print("Giving ",Item," to ",Who);
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