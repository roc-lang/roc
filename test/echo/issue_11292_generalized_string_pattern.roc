# A generalized string literal pattern, run under Boxy.
# https://github.com/roc-lang/roc/issues/11292
rank = |value|
    match value {
        "low" => 1
        "high" => 2
        _ => 3
    }

main! = |args| {
    low : Str
    low = if args.is_empty() "low" else "other"
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

    echo!(if first == 1 and second == 2 and rest == 3 "ok" else "wrong rank")
    Ok({})
}
