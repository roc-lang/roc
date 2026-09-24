# repro for https://github.com/roc-lang/roc/issues/11558
#
# `always_ok` returns the anonymous row `[Ok(num)]r`, and `keep_oks` uses it
# at the transparent nominal `Try(ok, _err)`. Every backend, including
# `--specialize=no`, keeps the Ok payloads.
keep_oks : List(a), (a -> Try(ok, _err)) -> List(ok)
keep_oks = |list, fun| {
    list.fold(
        [],
        |out_list, elem| {
            match fun(elem) {
                Ok(result) => out_list.append(result)
                Err(_) => out_list
            }
        },
    )
}

main! = |_args| {
    always_ok = |_| Ok(1)
    echo!("${Str.inspect(keep_oks([10], always_ok))}\n")
    Ok({})
}
