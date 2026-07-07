# repro for https://github.com/roc-lang/roc/issues/9875 (top-level variant)
# ThingAlias aliases Thing, so ThingAlias.from_u64 resolves to Thing's
# associated from_u64.
Thing := { n : U64 }.{
    from_u64 : U64 -> Thing
    from_u64 = |n| Thing.{ n }

    to_u64 : Thing -> U64
    to_u64 = |thing| thing.n
}

ThingAlias : Thing

expect Thing.to_u64(ThingAlias.from_u64(41)) == 41

main! = |_| {
    _ = ThingAlias.from_u64(1)
    Ok({})
}
