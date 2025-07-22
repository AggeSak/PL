fun sublist ls x = 
    let
        (* Traverse function to update accumulator based on element and threshold *)
        fun traverse (ell, [[]]) =
                if ell < x then [[ell]]
                else [[ell], []]
        
          | traverse (ell, (lastNum :: lastGroup) :: rest) =
                if (lastNum < x) andalso (ell < x) then
                    (ell :: lastNum :: lastGroup) :: rest
                else if (lastNum >= x) andalso (ell >= x) then
                    (ell :: lastNum :: lastGroup) :: rest    
                else
                    [ell] :: (lastNum :: lastGroup) :: rest

          | traverse (ell, acc) =
                [ell] :: acc  (* Catch-all pattern *)

        (* Initialize accumulator and process the list *)
        val initialAcc = [[]]
        val result = foldl traverse initialAcc ls
    in
        rev (map rev result)  (* Reverse sublists and the overall list *)
    end
