Pair(a, b) := { text: Str, left: List(a), right: List(b) }.{
    from_quote : Str -> Try(Pair(a, b), [BadQuotedBytes(Str)])
    from_quote = |text| {
        dbg text
        Ok({ text: Str.concat(text, "!"), left: [], right: [] })
    }

    encoder_for : fmt -> (Pair(a, b), state -> Try(state, err)) where [fmt.encode_str : Str, state -> Try(state, err)]
    encoder_for = |_encoding| |value, state| {
        Format : fmt
        text = if rank_pair(value) == 1 "yes" else "no"
        Format.encode_str(text, state)
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
    echo!(Json.to_str({ items: [first] }))
    echo!(Json.to_str({ items: [second] }))
    Ok({})
}
