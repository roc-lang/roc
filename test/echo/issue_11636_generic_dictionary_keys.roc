# A generic function whose dictionaries name its own type variable inside a
# compound key or element builds them at runtime from its frame under
# `--specialize=no`.
add_try : Set(Try(x, U64)), x -> U64
    where [x.is_eq : x, x -> Bool, x.to_hash : x, Hasher -> Hasher]
add_try = |s, x| s.insert(Ok(x)).insert(Ok(x)).len()

add_list : Set(List(x)), x -> U64
    where [x.is_eq : x, x -> Bool, x.to_hash : x, Hasher -> Hasher]
add_list = |s, x| s.insert([x]).len()

has_list : Set(List(x)), x -> Bool
    where [x.is_eq : x, x -> Bool, x.to_hash : x, Hasher -> Hasher]
has_list = |s, x| s.insert([x]).contains([x])

has_pair : List(x), x -> Bool
    where [x.is_eq : x, x -> Bool]
has_pair = |xs, b| xs.map(|a| (a, 1.U64)).contains((b, 1.U64))

main! = |_args| {
    echo!("${Str.inspect(add_try(Set.empty().insert(Ok("q")), "q"))}\n")
    echo!("${Str.inspect(add_try(Set.empty().insert(Err(3)), "q"))}\n")
    echo!("${Str.inspect(add_list(Set.empty().insert(["a"]), "b"))}\n")
    echo!("${Str.inspect(add_list(Set.empty().insert(["b"]), "b"))}\n")
    echo!("${Str.inspect(add_list(Set.empty().insert([1.U8]), 2.U8))}\n")
    echo!("${Str.inspect(add_list(Set.empty().insert([1.U64]), 1.U64))}\n")
    echo!("${Str.inspect(has_list(Set.empty(), 7.U8))}\n")
    echo!("${Str.inspect(has_pair(["a", "b"], "b"))}\n")
    echo!("${Str.inspect(has_pair(["a"], "b"))}\n")
    Ok({})
}
