# repro for https://github.com/roc-lang/roc/issues/11558
#
# `List.replace` runs its builtin inside a generic wrapper, so the elements
# are erased there and their reference counts follow the list's descriptor.
# Every backend, including `--specialize=no`, prints the same result.
main! = |args| {
    s = Str.repeat(Str.join_with(args, "x"), 30)
    base = [s, Str.concat(s, "b"), Str.concat(s, "c")]
    echo!("${Str.inspect(base.replace(1, s).map_ok(|r| r.list.len()))}\n")
    Ok({})
}
