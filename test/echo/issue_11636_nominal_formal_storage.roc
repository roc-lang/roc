# A nominal whose type argument is partly generic crosses a call boundary: the
# argument is converted into the callee's storage for that argument, and the
# result back into the caller's, under `--specialize=no`.
put_list : Dict(U64, List(x)), x -> Dict(U64, List(x))
    where [x.is_eq : x, x -> Bool, x.to_hash : x, Hasher -> Hasher]
put_list = |d, x| d.insert(3, [x, x])

put_pair : Dict(U64, (x, U64)), x -> U64
    where [x.is_eq : x, x -> Bool, x.to_hash : x, Hasher -> Hasher]
put_pair = |d, x| d.insert(3, (x, 2)).len()

put_value : Dict(U64, x), x -> U64
    where [x.is_eq : x, x -> Bool, x.to_hash : x, Hasher -> Hasher]
put_value = |d, x| d.insert(3, x).len()

main! = |_args| {
    lists = put_list(Dict.empty().insert(1, ["a"]), "b")
    echo!("${Str.inspect(lists.get(1))} ${Str.inspect(lists.get(3))} ${Str.inspect(lists.len())}\n")
    echo!("${Str.inspect(put_pair(Dict.empty().insert(1, ("a", 1)), "b"))}\n")
    echo!("${Str.inspect(put_value(Dict.empty().insert(1, "a"), "b"))}\n")
    Ok({})
}
