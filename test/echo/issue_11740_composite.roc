Pair(a, b) := { text: Str, left: List(a), right: List(b) }.{
    from_quote : Str -> Try(Pair(a, b), [BadQuotedBytes(Str)])
    from_quote = |text| {
        dbg text
        Ok({ text: Str.concat(text, "!"), left: [], right: [] })
    }

    is_eq : Pair(a, b), Pair(a, b) -> Bool
    is_eq = |x, y| x.text == y.text
}

rank_pair : Pair(a, b) -> U64
rank_pair = |value| match value {
    "low" => 1
    _ => 2
}

main! = |args| {
    first : Pair(Str, U64)
    first = if args.len() > 100 "x" else "low!"
    second : Pair(U64, Str)
    second = if args.len() > 100 "x" else "low!"
    echo!(Str.inspect(rank_pair(first)))
    echo!(Str.inspect(rank_pair(second)))
    Ok({})
}
