# A Nullable(a) descriptor holds a to_hash override whose where clause needs
# a.frob. Str has no frob, and is_eq's body has no a.frob in scope, yet both
# compose Nullable descriptors; neither hashes through them.
Foo := [Foo].{
    frob : Foo, Hasher -> Hasher
    frob = |_, hasher| hasher
}

Nullable(a) := [Null, NotNull(a)].{
    is_eq : Nullable(a), Nullable(a) -> Bool
    is_eq = |_, _y| Bool.True
    to_hash : Nullable(a), Hasher -> Hasher where [a.frob : a, Hasher -> Hasher]
    to_hash = |x, hasher| match x {
        NotNull(v) => v.frob(hasher)
        Null => hasher
    }
}

strs : List({ n : Nullable(Str) })
strs = [{ n: NotNull("a") }, { n: Null }]

foos : Dict({ n : Nullable(Foo) }, I64)
foos = Dict.empty().insert({ n: NotNull(Foo) }, 1)

main! = |_args| {
    echo!(Str.inspect(strs == [{ n: Null }]))
    echo!(Str.inspect(foos.len()))
    Ok({})
}
