Item := [Item(U64)].{
    score : Item -> U64
    score = |Item.Item(n)| n
}

Wrapper(a) : a

score_wrapped : Wrapper(a) -> U64 where [a.score : a -> U64]
score_wrapped = |value| value.score()

main! = |_args| {
    _ = score_wrapped(Item.Item(42))
    Ok({})
}
