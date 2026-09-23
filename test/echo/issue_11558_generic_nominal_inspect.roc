# repro for https://github.com/roc-lang/roc/issues/11558
#
# A nominal type with type parameters and a custom `to_inspect` is inspected
# through that method under every backend, including `--specialize=no`.
Wrap(a) :: { inner : a }.{
    to_inspect : Wrap(a) -> Str
    to_inspect = |w| "Wrap(${Str.inspect(w.inner)})"
}

main! = |_args| {
    w : Wrap(Str)
    w = { inner: "hi" }
    echo!("${Str.inspect(w)}\n")
    echo!("${Str.inspect(Dict.empty())}\n")
    echo!("${Str.inspect(Dict.from_list([("a", 1.U8)]))}\n")
    echo!("${Str.inspect(Set.empty())}\n")
    echo!("${Str.inspect(Set.from_list([2.I64]))}\n")
    Ok({})
}
