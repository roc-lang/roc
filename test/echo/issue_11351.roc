# repro for https://github.com/roc-lang/roc/issues/11351
empty = |_| []

main! = |_args| {
    unused = []
    _ = unused
    echo!("${Str.inspect(empty(1))}\n")
    echo!("${Str.inspect(List.len(empty(1)))}\n")
    echo!("${Str.inspect({ a: [], b: Err([]) })}\n")
    echo!("${Str.inspect(List.concat(empty(1), []))}\n")
    Ok([])
}
