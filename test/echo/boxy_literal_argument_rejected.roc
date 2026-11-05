# A literal a generic body converts at a type whose conversion rejects it is
# a compile error at the literal, the same under either strategy (design.md
# "Literal Arguments").
MyNum := { value: I64 }.{
    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
    from_numeral = |_numeral| Err(InvalidNumeral("nope"))

    plus : MyNum, MyNum -> MyNum
    plus = |a, b| { value: a.value + b.value }
}

add_one = |x| x.plus(1)

main! = |_| {
    five : MyNum
    five = { value: 5 }
    echo!(if add_one(five).value > 0 "big" else "small")
    Ok({})
}
