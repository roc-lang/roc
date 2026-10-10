capture = |value| |_| value
fixed_get : {} -> Str
fixed_get = |_| "other"
Word := { get: {} -> Str }.{
    from_quote : Str -> Try(Word, [BadQuotedBytes(Str)])
    from_quote = |text| {
        dbg text
        Ok({ get: if text == "other" fixed_get else capture(text) })
    }
    is_eq : Word, Word -> Bool
    is_eq = |x, y| (x.get)({}) == (y.get)({})
}
rank : a -> U64 where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), a.is_eq : a, a -> Bool]
rank = |value| match value {
    "low" => 1
    "other" => 2
    _ => 2
}
make_rank : a -> (a -> U64) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), a.is_eq : a, a -> Bool]
make_rank = |_| |value| rank(value)
main! = |args| {
    value = if args.len() > 100 "other".Word else "low".Word
    use_rank = make_rank(value)
    echo!(Str.inspect(use_rank(value)))
    Ok({})
}
