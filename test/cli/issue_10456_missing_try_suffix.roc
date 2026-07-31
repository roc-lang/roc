fallible : {} -> Try(Str, [FallibleErr])
fallible = |_| Ok("answer")

consume : Str -> Try({}, [ConsumeErr])
consume = |_| Ok({})

main! = |_args| {
    answer = fallible({})
    consume(answer)?
    Ok({})
}
