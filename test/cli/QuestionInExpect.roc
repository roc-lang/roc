QuestionInExpect := {}

to_positive : I64 -> Try(I64, [IsNegative])
to_positive = |x| {
    if x < 0 { Err(IsNegative) } else { Ok(x) }
}

expect {
    result = to_positive(3)?
    result == 3
}
