interface Quicksort
    exposes [swap, partition, quicksort]
    imports []

quicksort : List (Num a), U64, U64 -> List (Num a)
quicksort = \list, low, high ->
    when partition low high list is
        Pair partitionIndex partitioned ->
            partitioned
                |> quicksort low (partitionIndex - 1)
                |> quicksort (partitionIndex + 1) high


swap : U64, U64, List a -> List a
swap = \i, j, list ->
    when Pair (List.get list i) (List.get list j) is
        Pair (Ok atI) (Ok atJ) ->
            list
                |> List.set i atJ
                |> List.set j atI

        _ ->
            []


partition : U64, U64, List (Num a) -> [Pair U64 (List (Num a))]
partition = \low, high, initialList ->
    when List.get initialList high is
        Ok pivot ->
            when partitionHelp (low - 1) low initialList high pivot is
                Pair newI newList ->
                    Pair (newI + 1) (swap (newI + 1) high newList)

        Err _ ->
            Pair (low - 1) initialList


partitionHelp : U64, U64, List (Num a), U64, (Num a) -> [Pair U64 (List (Num a))]
partitionHelp = \i, j, list, high, pivot ->
    if j < high then
        when List.get list j is
            Ok value ->
                if value <= pivot then
                    partitionHelp (i + 1) (j + 1) (swap (i + 1) j list) high pivot
                else
                    partitionHelp i (j + 1) list high pivot

            Err _ ->
                Pair i list
    else
        Pair i list
