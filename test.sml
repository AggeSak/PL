fun test lst =
  let
    
    fun square x = x * x
  in
    
    map square ( map square lst)
  end;