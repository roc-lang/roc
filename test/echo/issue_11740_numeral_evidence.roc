MyNum := { value: I64 }.{
    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
    from_numeral = |_| {
        dbg "custom generic numeral"
        Ok({ value: 1 })
    }

    plus : MyNum, MyNum -> MyNum
    plus = |a, b| { value: a.value + b.value }

    to_i64 : MyNum -> I64
    to_i64 = |a| a.value
}

add_one = |x| x.plus(1)

forward : a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, a -> a]
forward = |value| add_one(value)

make_adder : a -> (a -> a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, a -> a]
make_adder = |_| |value| forward(value)

repeat_add : a, U64 -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, a -> a]
repeat_add = |value, remaining| if remaining == 0 value else repeat_add(forward(value), remaining - 1)

main! = |_| {
    five : MyNum
    five = { value: 5 }
    adder = make_adder(five)
    result = adder(five)
    echo!(result.to_i64().to_str())

    also : I64
    also = repeat_add(3, 2)
    echo!(also.to_str())

    Ok({})
}
