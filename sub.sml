fun allSums xs =
    let
        fun contiguousSublists [] = []
          | contiguousSublists (x::xs) =
              let
                  fun makeSublists [] = []
                    | makeSublists (y::ys) =
                        if y = x + 1 then
                            (x::y::ys) :: makeSublists ys
                        else
                            makeSublists ys
              in
                  [x] :: makeSublists xs @ contiguousSublists xs
              end
    in
        [] :: contiguousSublists xs
    end;


fun sumOfSublists (list_of_lists) =
  map (fn sublist => List.foldl (op +) 0 sublist) list_of_lists;

fun smallestAbsDiff (list : int list, target : int) =
  let
    fun helper (remaining_list : int list, current_min : int) =
      if null remaining_list then
        current_min
      else
        let
          val current_element = hd remaining_list
          val current_diff = abs(current_element -( target -current_element))
          val new_min = if current_diff < current_min then current_diff else current_min
        in
          helper (tl remaining_list, new_min)
        end
  in
    helper (list, abs(hd list - (target-hd list)))  (* Initialize min with difference of first element *)
  end;





