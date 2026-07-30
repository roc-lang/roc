Big := [Value].{
    from_numeral : Numeral -> Try(Big, [InvalidNumeral(Str)])
    from_numeral = |_| Ok(Value)
}

answer : Big
answer = 3e6000000000

main! = |_| {
    _ = answer
    Ok({})
}
