fun mapAllpairs f xs =
    let 
        fun h x y = (x,y)   
        fun g x = List.map (h x) xs
    in
        List.map f (List.map g xs )
   end;

fun mapAllpairss f xs =
    let
        fun h x = List.map (fn y => f (x, y)) xs
    in
        List.map h xs
    end;
