fun half []=([],[])
 | half [x] = ([x],[])
 | half (x :: y :: rest ) =
    let
     val (left,right) = half rest
    in
      (x :: left , y :: right)
    end

fun splitAt k [] = ([],[])
   | splitAt 0 l =([],l)
   | splitAt k l (h :: t) =
     let 
      val (left,right) = splitAt (k-1) t
     in  
       (h :: left,right)
    end  

fun split t=
let
  val n = length left
in
  splitAt (n div 2) t
end


fun assert cond =
   if not cond then print "wrong!\n" else ()


fun test_split f =(
  assert (f []=([],[])); 
  assert (f [42]=([42],[]));  
  assert (f [17,42]=([17],[42]))

) 