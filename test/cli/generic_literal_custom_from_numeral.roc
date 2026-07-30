MyNum := { value: I64 }.{
    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
    from_numeral = |_| Ok({ value: 1 })

    plus : MyNum, MyNum -> MyNum
    plus = |a, b| { value: a.value + b.value }

    to_i64 : MyNum -> I64
    to_i64 = |a| a.value
}

add_one = |x| x.plus(1)

main! = |_| {
    five : MyNum
    five = { value: 5 }
    result = add_one(five)
    echo!(result.to_i64().to_str())

    also : I64
    also = add_one(3)
    echo!(also.to_str())

    Ok({})
}
