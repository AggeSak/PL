fun loop 0 = ()
  |loop n =   (print (Int.toString n ^ "\n");loop (n - 1)) 
