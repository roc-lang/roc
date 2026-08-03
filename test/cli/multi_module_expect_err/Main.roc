import Helper

main! = |_args| Ok({})

to_positive : I64 -> Try(I64, [IsNegative])
to_positive = |x| {
    if x < 0 { Err(IsNegative) } else { Ok(x) }
}

expect {
    result = to_positive(-2)?
    result == 2
}
