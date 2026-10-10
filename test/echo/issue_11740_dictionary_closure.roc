keep_equal : a -> ({} -> Bool) where [a.is_eq : a, a -> Bool]
keep_equal = |value| |_| value == value
Word := { text: Str, check: {} -> Bool }.{
    from_quote : Str -> Try(Word, [BadQuotedBytes(Str)])
    from_quote = |text| {
        dbg text
        Ok({ text, check: keep_equal(text) })
    }
    is_eq : Word, Word -> Bool
    is_eq = |x, y| (x.check)({}) and (y.check)({}) and x.text == y.text
}
rank : a -> U64 where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), a.is_eq : a, a -> Bool]
rank = |value| match value {
    "low" => 1
    _ => 2
}
main! = |args| {
    value = if args.len() > 100 "other".Word else "low".Word
    echo!(Str.inspect(rank(value)))
    Ok({})
}
