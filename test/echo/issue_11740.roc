# repro for https://github.com/roc-lang/roc/issues/11740
#
# Under `--specialize=no` (Boxy), the pattern literal `"low"` inside the
# generic `rank` must be converted from a quoted literal to `Word` at compile
# time, exactly as every other build does: compile-time evaluation runs the
# conversion once (one `[dbg] "low"` line on stderr during the build) and the
# executable itself must run no user conversion code (no `[dbg] "low"` line
# while the program executes) and print `2`.

Word := { text: Str }.{
    is_eq : Word, Word -> Bool
    is_eq = |a, b| a.text == b.text

    from_quote : Str -> Try(Word, [BadQuotedBytes(Str)])
    from_quote = |text| {
        dbg text
        Ok({ text: Str.concat(text, "!") })
    }
}

rank : a -> U64 where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), a.is_eq : a, a -> Bool]
rank = |value| match value {
    "low" => 1
    _ => 2
}

main! = |args| {
    w = if args.len() > 100 "x".Word else "low!".Word
    echo!(Str.inspect(rank(w)))
    Ok({})
}
