# Derived `is_eq` and `to_hash` compare and hash each component with its own
# type's method: a nominal's custom method, `List`'s method, and a type
# variable's scheme requirement, at any nesting.
Loose := [L(U64, U64)].{
    is_eq : Loose, Loose -> Bool
    is_eq = |L(a, _), L(b, _)| a == b

    to_hash : Loose, Hasher -> Hasher
    to_hash = |L(a, _), h| a.to_hash(h)
}

Unannotated := [U(U64)].{
    is_eq = |U(a), U(b)| a == b
    to_hash = |U(a), h| a.to_hash(h)
}

mk : U64, U64 -> Loose
mk = |a, b| L(a, b)

same_pair : x, x -> Bool
    where [x.is_eq : x, x -> Bool]
same_pair = |a, b| (a, 1.U64) == (b, 1.U64)

count_pairs : x, x -> U64
    where [x.is_eq : x, x -> Bool, x.to_hash : x, Hasher -> Hasher]
count_pairs = |a, b| Set.empty().insert((a, 1.U64)).insert((b, 1.U64)).len()

list_pair : Set((List(x), U64)), x -> U64
    where [x.is_eq : x, x -> Bool, x.to_hash : x, Hasher -> Hasher]
list_pair = |s, x| s.insert(([x], 1)).len()

nested : Set(List(List(x))), x -> U64
    where [x.is_eq : x, x -> Bool, x.to_hash : x, Hasher -> Hasher]
nested = |s, x| s.insert([[x]]).len()

rec : Set({ a : x, b : List(x) }), x -> U64
    where [x.is_eq : x, x -> Bool, x.to_hash : x, Hasher -> Hasher]
rec = |s, x| s.insert({ a: x, b: [x] }).len()

tagged : Set([One(x), Two(List(x), U64)]), x -> U64
    where [x.is_eq : x, x -> Bool, x.to_hash : x, Hasher -> Hasher]
tagged = |s, x| s.insert(Two([x], 3)).insert(One(x)).len()

eq_nested : x, x -> Bool
    where [x.is_eq : x, x -> Bool]
eq_nested = |a, b| ([[a]], { f: a }) == ([[b]], { f: b })

main! = |_args| {
    echo!("${Str.inspect((mk(1, 2), 1.U64) == (mk(1, 3), 1.U64))}\n")
    echo!("${Str.inspect(same_pair(mk(1, 2), mk(1, 3)))}\n")
    echo!("${Str.inspect(Set.empty().insert((mk(1, 2), 1.U64)).insert((mk(1, 3), 1.U64)).len())}\n")
    echo!("${Str.inspect(count_pairs(mk(1, 2), mk(1, 3)))}\n")
    echo!("${Str.inspect(Set.empty().insert((U(1), 1.U64)).insert((U(1), 1.U64)).len())}\n")
    echo!("${Str.inspect(Set.empty().insert((["a"], 1.U64)).insert((["a"], 1.U64)).len())}\n")
    echo!("${Str.inspect(Set.empty().insert(([["a"]], 1.U64)).insert(([["a"]], 1.U64)).len())}\n")
    echo!("${Str.inspect(list_pair(Set.empty().insert((["a"], 1)), "b"))}\n")
    echo!("${Str.inspect(nested(Set.empty().insert([["a"]]), "a"))}\n")
    echo!("${Str.inspect(nested(Set.empty().insert([[mk(1, 2)]]), mk(1, 3)))}\n")
    echo!("${Str.inspect(rec(Set.empty().insert({ a: "a", b: ["a"] }), "b"))}\n")
    echo!("${Str.inspect(tagged(Set.empty().insert(One("a")), "a"))}\n")
    echo!("${Str.inspect(eq_nested(mk(1, 2), mk(1, 3)))}\n")
    echo!("${Str.inspect(eq_nested(mk(1, 2), mk(2, 2)))}\n")
    Ok({})
}
