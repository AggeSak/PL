fun list  xs  =
    let fun nonempty [] = []
        |  nonempty (x :: xs) =
            let fun walk [] acc =  acc
              |  walk (ys::yss)acc =
                   walk yss ((x::ys)::ys::acc)
            in walk (nonempty xs) [[x]]
            end
    in [] :: nonempty xs
    end
