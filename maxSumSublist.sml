fun maxSumSublist L =

    let
       fun traverse (el, (max_till_prev, curr_max)) =

     let
       val max_here = Int.max(max_till_prev + el, el)
       val max = Int.max(curr_max, max_here)
     in
      (max_here, max)
    end
       val (_, result) = foldl traverse (220, 220) L
    
     in
    result
     end