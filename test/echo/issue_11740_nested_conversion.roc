Word := { text: Str }.{
    from_quote : Str -> Try(Word, [BadQuotedBytes(Str)])
    from_quote = |text| {
        dbg text
        Ok({ text: text })
    }
    is_eq : Word, Word -> Bool
    is_eq = |a, b| a.text == b.text
}
Wrapped(a) := { text: Str, value: a }.{
    from_quote : Str -> Try(Wrapped(a), [BadQuotedBytes(Str)]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]
    from_quote = |text| {
        dbg text
        value : a
        value = "inner"
        Ok({ text: text, value: value })
    }
    is_eq : Wrapped(a), Wrapped(a) -> Bool
    is_eq = |x, y| x.text == y.text
}
rank : Wrapped(a) -> U64 where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]
rank = |value| match value {
    "outer" => 1
    _ => 2
}
rank_any : a -> U64 where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), a.is_eq : a, a -> Bool]
rank_any = |value| match value {
    "outer" => 1
    _ => 2
}

main! = |args| {
    value : Wrapped(Word)
    value = if args.len() > 100 "unused" else "input"
    echo!(Str.inspect(rank(value)))
    echo!(Str.inspect(rank_any(value)))
    Ok({})
}
