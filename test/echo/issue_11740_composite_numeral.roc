Count(a) := { value: U64, items: List(a) }.{
    from_numeral : Numeral -> Try(Count(a), [InvalidNumeral(Str)])
    from_numeral = |_| {
        dbg "custom numeral"
        Ok({ value: 7, items: [] })
    }

    is_eq : Count(a), Count(a) -> Bool
    is_eq = |x, y| x.value == y.value
}

rank : Count(a) -> U64
rank = |value| match value {
    42 => 1
    _ => 2
}

main! = |args| {
    n = if args.len() > 100 8 else 7
    first : Count(Str)
    first = { value: n, items: [] }
    second : Count(U64)
    second = { value: n, items: [] }
    echo!(Str.inspect(rank(first)))
    echo!(Str.inspect(rank(second)))
    Ok({})
}
