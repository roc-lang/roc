app [main!] { pf: platform "./platform/main.roc" }

Word := { code: U64 }.{
    from_quote : Str -> Try(Word, [BadQuotedBytes(Str)])
    from_quote = |text|
        if text == "never" {
            crash "a pattern conversion was evaluated"
        } else if text == "low" {
            Ok({ code: 1 })
        } else {
            Ok({ code: 3 })
        }

    is_eq : Word, Word -> Bool
    is_eq = |a, b| a.code == b.code
}

short_circuit = |value| match value {
    "low" => 1
    "never" => 2
    _ => 3
}

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| {
    short : U64
    short = short_circuit("low".Word)
    if short == 1 Ok({}) else Err(Exit(1))
}
