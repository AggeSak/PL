fun walk [] acc x = List.rev acc
              |  walk (ys::yss)acc =
                   walk yss ((x::ys)::ys::acc)