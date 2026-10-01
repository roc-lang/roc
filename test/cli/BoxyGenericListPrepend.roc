BoxyGenericListPrepend := {}

# repro for https://github.com/roc-lang/roc/issues/11870: prepending to a list
# of a type variable passes the list descriptor to the Boxy runtime.
f : List(a), a -> List(a)
f = |l, x| List.prepend(l, x)

expect f([], "a") == ["a"]
expect f(["b", "c"], "a") == ["a", "b", "c"]
expect f([[1, 2]], [3]) == [[3], [1, 2]]
expect f([1.U8, 2], 0) == [0, 1, 2]
expect List.len(f(f([], "y"), "x")) == 2
