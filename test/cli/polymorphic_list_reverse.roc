import PolymorphicListReverseMod

main! = |_args| {
    echo!("ok")
    Ok({})
}

expect {
    PolymorphicListReverseMod.reverse([2]) == [2]
}
