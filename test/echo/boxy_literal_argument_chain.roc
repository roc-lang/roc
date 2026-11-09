# Literal arguments (design.md "Literal Arguments"): a literal in a generic
# body converts where its type first becomes concrete, under either strategy.
# Each conversion at MyNum runs once, at compile time: the `1` of the local
# `add_one` forwarded through `add_one_twice`, and the `2` of
# `Wrapper.from_numeral` reached while `wrapped`'s `5 : Wrapper(MyNum)`
# converts.
MyNum := { value: I64 }.{
    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
    from_numeral = |_numeral| {
        dbg "convert"
        Ok({ value: 7 })
    }

    plus : MyNum, MyNum -> MyNum
    plus = |a, b| { value: a.value + b.value }

    to_i64 : MyNum -> I64
    to_i64 = |a| a.value
}

Wrapper(a) := [W(a)].{
    from_numeral : Numeral -> Try(Wrapper(a), [InvalidNumeral(Str)]) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]
    from_numeral = |_numeral| Ok(Wrapper.W(2))

    unwrap : Wrapper(a) -> a
    unwrap = |Wrapper.W(inner)| inner
}

# A literal whose type is a compound over the scheme's variable.
wrapped : a -> Wrapper(a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]
wrapped = |_| 5

# A literal in a generalized local function, forwarded through its use.
add_one_twice = |x| {
    add_one = |y| y.plus(1)
    add_one(add_one(x))
}

main! = |_| {
    five : MyNum
    five = { value: 5 }
    echo!(add_one_twice(five).to_i64().to_str())
    echo!(wrapped(five).unwrap().to_i64().to_str())
    also : I64
    also = add_one_twice(3.I64)
    echo!(also.to_str())
    w : Wrapper(I64)
    w = wrapped(1.I64)
    echo!(w.unwrap().to_str())
    Ok({})
}
