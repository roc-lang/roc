# repro for https://github.com/roc-lang/roc/issues/11636
#
# A partially generic container (`Dict(U64, List(x))`) crosses a call boundary.
# Under `--specialize=no` (Boxy lowering) this must run and produce the correct
# result, like it already does under the default specialization strategy.
f : Dict(U64, List(x)), x -> U64
    where [x.is_eq : x, x -> Bool, x.to_hash : x, Hasher -> Hasher]
f = |d, x| d.insert(3, [x]).len()

main! = |_args| {
    echo!("${Str.inspect(f(Dict.empty().insert(1, ["a"]), "b"))}\n")
    Ok({})
}
