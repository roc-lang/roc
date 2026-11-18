# https://github.com/roc-lang/roc/issues/13101
Counter := {}.{
    restart : Counter, Stream(U8) -> Stream(U8)
    restart = |_c, source| Stream.custom(source, (0.U64).range_len_if_known(0, 1, Inclusive), |s| step!(s))
}

step! : Stream(U8) => Try((U8, Stream(U8)), [NoMore])
step! = |s|
    match s.next!() {
        One({ item, rest: _ }) => Ok((item, Stream.from_iter([].iter())))
        _ => Err(NoMore)
    }

main! = |_args| {
    n = Counter.{}.restart(Stream.from_iter([1].iter())).collect!().len()
    echo!("${n.to_str()}\n")
    Ok({})
}
