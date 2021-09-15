app "reverse"
    packages { base: "platform" }
    imports []
    provides [ quicksort ] to base

# the platform expects a function with this name
quicksort : List I64 -> List I64
quicksort = \input -> 
    sortImmutable input

sortInPlace = \input ->
    sortingNetwork input

sortImmutable = \input ->
    sorted = sortingNetwork input

    if 1 == 1 then
        sorted
    else
        # make morphic believe we may use the input
        # hence, it cannot be update in-place
        input


# optimal sorting network for 10 elements
# https://stackoverflow.com/questions/32172144/fastest-way-to-sort-10-numbers-numbers-are-32-bit
sortingNetwork = \input ->
    input
        |> swapIf 0 5
        |> swapIf 1 6
        |> swapIf 2 7
        |> swapIf 3 8
        |> swapIf 4 9
        |> swapIf 0 3
        |> swapIf 5 8
        |> swapIf 1 4
        |> swapIf 6 9
        |> swapIf 0 2
        |> swapIf 3 6
        |> swapIf 7 9
        |> swapIf 0 1
        |> swapIf 2 4
        |> swapIf 5 7
        |> swapIf 8 9
        |> swapIf 1 2
        |> swapIf 3 5
        |> swapIf 4 6
        |> swapIf 7 8
        |> swapIf 1 3
        |> swapIf 4 7
        |> swapIf 2 5
        |> swapIf 6 8
        |> swapIf 2 3
        |> swapIf 4 5
        |> swapIf 6 7
        |> swapIf 3 4
        |> swapIf 5 6
   
swapIf = \list, p, q ->
    a = List.get list p
    b = List.get list q

    when Pair a b is
        Pair (Ok x) (Ok y) if x > y ->
            List.swap list p q

        _ ->
            # trick morphic into believing we've made a new list
            List.swap list 0 0
