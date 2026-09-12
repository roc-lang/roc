app [main!] { pf: platform "./platform/main.roc" }

# repro for https://github.com/roc-lang/roc/issues/11292
rank = |value|
    match value {
        "low" => 1
        "high" => 2
        _ => 3
    }

main! : List(Str) => Try({}, [Exit(I8)])
main! = |_args| {
    low : Str
    low = "low"
    high : Str
    high = "high"
    other : Str
    other = "other"

    first : U64
    first = rank(low)
    second : U64
    second = rank(high)
    rest : U64
    rest = rank(other)

    if first == 1 and second == 2 and rest == 3 Ok({}) else Err(Exit(1))
}
