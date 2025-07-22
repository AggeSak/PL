fun mingle lists =
    let
        fun aux ([], _) = []
          | aux (lists, n) =
              let
                  val current = List.map (fn lst => if n < List.length lst then List.nth(lst, n) else NONE) lists
                  val filtered = List.filter (fn x => case x of NONE => false | SOME _ => true) current
              in
                  List.map Option.valOf filtered @ aux (lists, n + 1)
              end
    in
        aux (lists, 0)
    end
