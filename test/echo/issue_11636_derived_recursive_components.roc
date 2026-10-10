Loose := [L(U64, U64)].{
    is_eq : Loose, Loose -> Bool
    is_eq = |L(a, _), L(b, _)| a == b

    to_hash : Loose, Hasher -> Hasher
    to_hash = |L(a, _), h| a.to_hash(h)
}

mk : U64, U64 -> Loose
mk = |a, b| L(a, b)

Foo(a) := [Bar(List(a)), Baz].{
    is_eq : _
    to_hash : _
}

Deep(a) := [D(List((a, U64)))].{
    is_eq : _
    to_hash : _
}

One(a) := [One(a), NoOne].{
    is_eq : _
    to_hash : _
}

Stack(a) := [Empty, Push(a, Stack(a))].{
    is_eq : _
    to_hash : _
}

Tree := [Leaf, Node(Tree, U64, Tree)].{
    is_eq : _
    to_hash : _
}

sum : Tree -> U64
sum = |t| match t {
    Leaf => 0
    Node(l, v, r) => sum(l) + v + sum(r)
}

Opt(a) := [Some(a), Nothing].{
    is_eq : _
}

main! = |args| {
    n = List.len(args) + 1

    f1 : Foo(Loose)
    f1 = Bar([mk(n, 2)])
    f2 : Foo(Loose)
    f2 = Bar([mk(n, 3)])
    d1 : Deep(Loose)
    d1 = D([(mk(n, 2), 1)])
    d2 : Deep(Loose)
    d2 = D([(mk(n, 3), 1)])
    o1 : One(Loose)
    o1 = One(mk(n, 2))
    o2 : One(Loose)
    o2 = One(mk(n, 3))
    s1 : Stack(Loose)
    s1 = Push(mk(n, 2), Push(mk(n, 4), Empty))
    s2 : Stack(Loose)
    s2 = Push(mk(n, 3), Push(mk(n, 5), Empty))
    s3 : Stack(Loose)
    s3 = Push(mk(n, 3), Empty)
    t1 : Tree
    t1 = Node(Leaf, n, Node(Leaf, 2, Leaf))
    t2 : Tree
    t2 = Node(Leaf, n, Node(Leaf, 3, Leaf))

    echo!("${Str.inspect(f1 == f2)} ${Str.inspect(d1 == d2)} ${Str.inspect(o1 == o2)}\n")
    echo!("${Str.inspect(s1 == s2)} ${Str.inspect(s1 == s3)} ${Str.inspect(t1 == t2)} ${Str.inspect(t1 == t1)}\n")
    echo!("${Str.inspect(Opt.Some(n + 1) == Opt.Some(2))} ${Str.inspect(Opt.Some(1) == Opt.Nothing)}\n")
    echo!("${Str.inspect(Set.empty().insert(f1).insert(f2).len())} ${Str.inspect(Set.empty().insert(d1).insert(d2).len())} ${Str.inspect(Set.empty().insert(o1).insert(o2).len())}\n")
    echo!("${Str.inspect(Set.empty().insert(s1).insert(s2).insert(s3).len())} ${Str.inspect(Set.empty().insert(t1).insert(t2).insert(t1).len())}\n")
    echo!("${Str.inspect(sum(t1))} ${Str.inspect(sum(t2))}\n")
    Ok({})
}
