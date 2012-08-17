last_order_id(1). 


+!order(Product,Qtd)[source(Ag,Arch)] : last_order_id(N) &
   OrderId = N +1
  <- 
     -+last_order_id(OrderId);
     .print("Sending ", Qtd, " units of ", Product, "to ", Arch);
     .send(fridge, Arch,tell, delivered(Product,Qtd,OrderId)).

