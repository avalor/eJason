last_order_id(1). 

!start.

+!start <-
  .print("Supermarket is open.").

+!order(Product,Qtd)[source(Ag)] : last_order_id(N) &
   OrderId = N +1
  <- 
     -+last_order_id(OrderId);
     .print("Sending ", Qtd, " units of ", Product, " to fridge as requested by ", Ag);
     .send(fridge,tell, delivered(Product,Qtd,OrderId)).

