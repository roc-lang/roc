app "nqueens"
    packages { pf: "platform/main.roc" }
    imports [pf.Task]
    provides [main] to pf

main : Task.Task {} []
main =
    Task.after
        Task.getInt
        \n ->
            queens n # original koka 13
            |> Num.toStr
            |> Task.putLine

ConsList a : [Nil, Cons a (ConsList a)]

queens = \n -> length (findSolutions n n)

length : ConsList a -> I64
length = \xs -> lengthHelp xs 0

lengthHelp : ConsList a, I64 -> I64
lengthHelp = \foobar, acc ->
    when foobar is
        Cons _ lrest -> lengthHelp lrest (1 + acc)
        Nil -> acc

safe : I64, I64, ConsList I64 -> Bool
safe = \queen, diagonal, xs ->
    when xs is
        Nil -> Bool.true
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

extend = \n, acc, solutions ->
    when solutions is
        Nil -> acc
        Cons soln rest -> extend n (appendSafe n soln acc) rest

findSolutions = \n, k ->
    if k == 0 then
        Cons Nil Nil
    else
        extend n Nil (findSolutions n (k - 1))
