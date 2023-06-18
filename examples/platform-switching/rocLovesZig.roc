app "rocLovesZig"
    packages { pf: "zig-platform/main.roc" }
    imports []
    provides [main] to pf

LinkedList a : [Nil, Cons a (LinkedList a)]

map : LinkedList a, (a -> b) -> LinkedList b
map = \list, f ->
    when list is
        Nil -> Nil
        Cons x xs -> Cons (f x) (map xs f)

unfold : a, Nat -> LinkedList a
unfold = \value, n ->
    when n is
        0 -> Nil
        _ -> Cons value (unfold value (n - 1))

length : LinkedList a -> I64
length = \list ->
    when list is
        Nil -> 0
        Cons _ rest -> 1 + length rest

main : Str 
main = 
    unfold 42 5
        |> map (\x -> x + 1i64) 
        |> length
        |> Num.toStr
