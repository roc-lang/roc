app "effect-example"
    packages { base: "thing/platform-dir" }
    imports [base.Task]
    provides [ main ] to base

main : Task.Task {} []
main =
    queens 10
        |> Str.fromInt
        |> Task.putLine

ConsList a : [ Nil, Cons a (ConsList a) ]

queens = \n -> length (findSolutions n n)


length : ConsList a -> I64
length = \xs -> lengthHelp xs 0

lengthHelp : ConsList a, I64 -> I64
lengthHelp = \xs, acc ->
    when xs is
        Nil -> acc
        Cons _ rest -> lengthHelp rest (1 + acc)

safe : I64, I64, ConsList I64 -> Bool
safe = \queen, diagonal, xs ->
    when xs is
        Nil ->
            True

        Cons q t ->
            queen != q && queen != q + diagonal && queen != q - diagonal && safe queen (diagonal + 1) t

appendSafe : I64, ConsList I64, ConsList (ConsList I64) -> ConsList (ConsList I64)
appendSafe = \k, soln, solns ->
    if k <= 0 then
        solns
    else if safe k 1 soln then
        appendSafe (k - 1) soln (Cons (Cons k soln) solns)
    else
        appendSafe (k - 1) soln solns

extend = \n, acc, solns ->
    when solns is
        Nil -> acc
        Cons soln rest -> extend n (appendSafe n soln acc) rest

findSolutions = \n, k ->
    if k == 0 then
        Cons Nil Nil

    else
        extend n Nil (findSolutions n (k - 1))
