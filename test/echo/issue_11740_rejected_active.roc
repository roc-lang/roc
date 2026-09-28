Word := { text: Str }.{
    is_eq : Word, Word -> Bool
    is_eq = |a, b| a.text == b.text

    from_quote : Str -> Try(Word, [BadQuotedBytes(Str)])
    from_quote = |text| {
        dbg text
        if text == "low" { Err(BadQuotedBytes("literal-boom")) } else { Ok({ text: Str.concat(text, "!") }) }
    }
}

rank : a -> U64 where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), a.is_eq : a, a -> Bool]
rank = |value| match value {
    "low" => 1
    _ => 2
}

make_rank : a -> (a -> U64) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), a.is_eq : a, a -> Bool]
make_rank = |_| |value| rank(value)

main! = |args| {
    value = "runtime".Word
    callback = make_rank(value)
    if args.len() < 100 {
        echo!(Str.inspect(callback(value)))
    } else {
        echo!("dormant")
    }
    Ok({})
}
