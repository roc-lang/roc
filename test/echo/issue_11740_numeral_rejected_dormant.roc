Number := { value: U64 }.{
    from_numeral : Numeral -> Try(Number, [InvalidNumeral(Str)])
    from_numeral = |_| {
        dbg "rejected numeral"
        Err(InvalidNumeral("numeral-boom"))
    }
    is_eq : Number, Number -> Bool
    is_eq = |x, y| x.value == y.value
}
rank : a -> U64 where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_eq : a, a -> Bool]
rank = |value| match value {
    1 => 1
    _ => 2
}
make_rank : a -> (a -> U64) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_eq : a, a -> Bool]
make_rank = |_| |value| rank(value)
main! = |args| {
    value : Number
    value = { value: args.len().to_u64() }
    callback = make_rank(value)
    if args.len() > 100 {
        echo!(Str.inspect(callback(value)))
    } else {
        echo!("dormant")
    }
    Ok({})
}
