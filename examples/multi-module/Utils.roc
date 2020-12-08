interface Utils exposes [ swap ] imports []

swap : I64, I64, List a -> List a
swap = \i, j, list ->
    when Pair (List.get list i) (List.get list j) is
        Pair (Ok atI) (Ok atJ) ->
            list 
                |> List.set i atJ
                |> List.set j atI

        _ ->
            []
