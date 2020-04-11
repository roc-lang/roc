app Quicksort provides [ quicksort ] imports []


quicksort : List (Num a) -> List (Num a)
quicksort = \list ->
    quicksortHelp list 0 (List.len list - 1)


quicksortHelp : List (Num a), Int, Int -> List (Num a)
quicksortHelp = \list, low, high ->
    when partition low high list is
        Pair partitionIndex partitioned ->
            partitioned
                |> quicksortHelp low (partitionIndex - 1)
                |> quicksortHelp (partitionIndex + 1) high


swap : Int, Int, List a -> List a
swap = \i, j, list ->
    when Pair (List.get list i) (List.get list j) is
        Pair (Ok atI) (Ok atJ) ->
            list
                |> List.set i atJ
                |> List.set j atI

        _ ->
            []


partition : Int, Int, List (Num a) -> [ Pair Int (List (Num a)) ]
partition = \low, high, initialList ->
    when List.get initialList high is
        Ok pivot ->
            go = \i, j, list ->
                if j < high then
                    when List.get list j is
                        Ok value ->
                            if value <= pivot then
                                go (i + 1) (j + 1) (swap (i + 1) j list)
                            else
                                go i (j + 1) list

                        Err _ ->
                            Pair i list
                else
                    Pair i list

            when go (low - 1) low initialList is
                Pair newI newList ->
                    Pair (newI + 1) (swap (newI + 1) high newList)

        Err _ ->
            Pair (low - 1) initialList

