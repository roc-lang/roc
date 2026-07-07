# Associated aliases that form a cycle must report an error (not loop or
# panic) when an associated item is looked up through them.
Api :: [].{
    AliasA : AliasB
    AliasB : AliasA
}

main! = |_| {
    _ = Api.AliasA.from_u64(1)
    Ok({})
}
