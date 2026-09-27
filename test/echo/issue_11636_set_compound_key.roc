# repro for https://github.com/roc-lang/roc/issues/11636
#
# A generic function stores its own type variable inside a compound `Set` key.
# Under `--specialize=no` (Boxy lowering) this must run and produce the correct
# result, like it already does under the default specialization strategy.
add_try : Set(Try(x, U64)), x -> U64
    where [x.is_eq : x, x -> Bool, x.to_hash : x, Hasher -> Hasher]
add_try = |s, x| s.insert(Ok(x)).len()

main! = |_args| {
    echo!("${Str.inspect(add_try(Set.empty().insert(Err(3)), "q"))}\n")
    Ok({})
}
