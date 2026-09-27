app [main!] { pf: platform "./platform/main.roc" }

Word := { text: Str }.{
    is_eq : _
    from_quote : Str -> Try(Word, [BadQuotedBytes(Str)])
    from_quote = |text|
        if text == "reject" Err(BadQuotedBytes(text)) else Ok({ text: text })
}

# Only the `expect` below specializes this match at Word.
matches = |value| match value {
    "reject" => True
    _ => False
}

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| Ok({})

expect matches("valid".Word) == False
