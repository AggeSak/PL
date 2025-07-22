fun sumColumns lists =
    let
        (* Helper function to transpose the list, stopping when any list becomes empty *)
        fun transpose rows =
            if exists null rows then []
            else map hd rows :: transpose (map tl rows)
        
        (* Summing each transposed row (which represents original column) *)
        fun sumList lst = foldl (op +) 0 lst
    in
        map sumList (transpose lists)
    end;
