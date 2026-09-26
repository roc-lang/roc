## A where-call inside a generic Boxy worker whose receiver type mentions the
## worker's own type variable (`count(l)` with `l : List(x)`): the dictionary
## is built in the worker's frame, which describes `x`.
GenericWorkerDictionary := {}

Wrap(a) := [W(List(a))].{
    size : Wrap(a) -> U64
    size = |W(l)| List.len(l)
}

count : a -> U64 where [a.len : a -> U64]
count = |x| x.len()

sized : a -> U64 where [a.size : a -> U64]
sized = |x| x.size()

h : List(x) -> U64
h = |l| count(l)

hw : Wrap(x) -> U64
hw = |w| sized(w)

pair : List(x), List(y) -> U64
pair = |a, b| count(a) + count(b)

expect h(["a", "b"]) == 2
expect h([1.I64, 2, 3]) == 3
expect hw(W(["a", "b"])) == 2
expect pair(["a"], [1.I64, 2]) == 3
