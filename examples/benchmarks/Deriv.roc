app "deriv"
    packages { base: "thing/platform-dir" }
    imports [base.Task]
    provides [ main ] to base

# based on: https://github.com/koka-lang/koka/blob/master/test/bench/haskell/deriv.hs

main : Task.Task {} []
main =
    queens 10
        |> Str.fromInt
        |> Task.putLine

Expr : [ Val I64, Var Str, Add Expr Expr, Mul Expr Expr, Pow Expr Expr, Ln Expr ]

pown : I64, I64 -> I64
pown = \a, n ->
    when n is
        0 -> 1
        1 -> a
        _ ->
            b = pown a (n / 2)
            b * b * (if n % 2 == 0 then 1 else a)


