app "quicksort" imports [ Utils.{ swap } ] provides [ quicksort ] to "./platform"


quicksort : List I64 -> List I64
quicksort = \originalList ->
    quicksortHelp : List (Num a), I64, I64 -> List (Num a)
    quicksortHelp = \list, low, high ->
        if low < high then
            when partition low high list is
                Pair partitionIndex partitioned ->
                    partitioned
                        |> quicksortHelp low (partitionIndex - 1)
                        |> quicksortHelp (partitionIndex + 1) high
        else
            list

    partition : I64, I64, List (Num a) -> [ Pair I64 (List (Num a)) ]
    partition = \low, high, initialList ->
        when List.get initialList high is
            Ok pivot ->
                when partitionHelp (low - 1) low initialList high pivot is
                    Pair newI newList ->
                        Pair (newI + 1) (Utils.swap (newI + 1) high newList)

            Err _ ->
                Pair (low - 1) initialList


    partitionHelp : I64, I64, List (Num a), I64, (Num a) -> [ Pair I64 (List (Num a)) ]
    partitionHelp = \i, j, list, high, pivot ->
        if j < high then
            when List.get list j is
                Ok value ->
                    if value <= pivot then
                        partitionHelp (i + 1) (j + 1) (Utils.swap (i + 1) j list) high pivot
                    else
                        partitionHelp i (j + 1) list high pivot

                Err _ ->
                    Pair i list
        else
            Pair i list



    n = List.len originalList
    quicksortHelp originalList 0 (n - 1)
