interface Quicksort exposes [sortBy, sortWith, show]

show : List I64 -> Str
show = \list ->
    if List.isEmpty list then
        "[]"
    else
        content =
            list
            |> List.map Num.toStr
            |> Str.joinWith ", "

        "[\(content)]"

sortBy : List a, (a -> Num *) -> List a
sortBy = \list, toComparable ->
    sortWith list (\x, y -> Num.compare (toComparable x) (toComparable y))

Order a : a, a -> [LT, GT, EQ]

sortWith : List a, (a, a -> [LT, GT, EQ]) -> List a
sortWith = \list, order ->
    n = List.len list

    quicksortHelp list order 0 (n - 1)

quicksortHelp : List a, Order a, Nat, Nat -> List a
quicksortHelp = \list, order, low, high ->
    if low < high then
        when partition low high list order is
            Pair partitionIndex partitioned ->
                partitioned
                |> quicksortHelp order low (Num.subSaturated partitionIndex 1)
                |> quicksortHelp order (partitionIndex + 1) high
    else
        list

partition : Nat, Nat, List a, Order a -> [Pair Nat (List a)]
partition = \low, high, initialList, order ->
    when List.get initialList high is
        Ok pivot ->
            when partitionHelp low low initialList order high pivot is
                Pair newI newList ->
                    Pair newI (swap newI high newList)

        Err _ ->
            Pair low initialList

partitionHelp : Nat, Nat, List c, Order c, Nat, c -> [Pair Nat (List c)]
partitionHelp = \i, j, list, order, high, pivot ->
    if j < high then
        when List.get list j is
            Ok value ->
                when order value pivot is
                    LT | EQ ->
                        partitionHelp (i + 1) (j + 1) (swap i j list) order high pivot

                    GT ->
                        partitionHelp i (j + 1) list order high pivot

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
            []
