app "quicksort"
    packages { pf: "platform" }
    imports []
    provides [ quicksort ] to pf

quicksort = \originalList ->
    n = List.len originalList

    quicksortHelp originalList 0 (n - 1)

quicksortHelp : List (Num a), Nat, Nat -> List (Num a)
quicksortHelp = \list, low, high ->
    if low < high then
        when partition low high list is
            Pair partitionIndex partitioned ->
                partitioned
                    |> quicksortHelp low (partitionIndex - 1)
                    |> quicksortHelp (partitionIndex + 1) high
    else
        list

partition : Nat, Nat, List (Num a) -> [ Pair Nat (List (Num a)) ]
partition = \low, high, initialList ->
    when List.get initialList high is
        Ok pivot ->
            when partitionHelp (low - 1) low initialList high pivot is
                Pair newI newList ->
                    Pair (newI + 1) (swap (newI + 1) high newList)

        Err _ ->
            Pair (low - 1) initialList

partitionHelp : Nat, Nat, List (Num c), Nat, Num c -> [ Pair Nat (List (Num c)) ]
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

swap : Nat, Nat, List a -> List a
swap = \i, j, list ->
    when Pair (List.get list i) (List.get list j) is
        Pair (Ok atI) (Ok atJ) ->
            list
                |> List.set i atJ
                |> List.set j atI

        _ ->
            # to prevent a decrement on list
            # turns out this is very important for optimizations
            list
