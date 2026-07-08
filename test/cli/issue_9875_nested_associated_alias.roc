# repro for https://github.com/roc-lang/roc/issues/9875 (nested variant)
# ThingAlias is an associated alias of Api pointing at Thing, so
# Api.ThingAlias.from_u64 must resolve to Thing's associated from_u64.
Thing := { n : U64 }.{
    from_u64 : U64 -> Thing
    from_u64 = |n| Thing.{ n }

    to_u64 : Thing -> U64
    to_u64 = |thing| thing.n
}

Api :: [].{
    ThingAlias : Thing
}

expect Thing.to_u64(Api.ThingAlias.from_u64(41)) == 41

main! = |_| {
    _ = Api.ThingAlias.from_u64(1)
    Ok({})
}
