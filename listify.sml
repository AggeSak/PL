fun listify ls x =
    let
        (* Traverse function that updates the accumulator based on element and comparison to x *)
        fun traverse (el, []) =
            if el > x then [[el]]
            else [[el]]
         
         | traverse (el, (lastGroup :: rest)) =
            if el > x andalso hd lastGroup > x then
                (* Both current and last element are greater than x, add to current group *)
                (el :: lastGroup) :: rest
            else if el > x andalso hd lastGroup <= x then
                (* Current element is greater than x, but lastGroup starts with a value <= x,
                   so start a new group *)
                [el] :: (lastGroup :: rest)
            else
                (* Current element is <= x, just add it to the last group *)
                (el :: lastGroup) :: rest

        (* Initial accumulator *)
        val acc = [[]]

        (* Use foldl to apply the traverse function over the list *)
        val result = foldl traverse acc ls
    in
      rev( map rev result)
    end
