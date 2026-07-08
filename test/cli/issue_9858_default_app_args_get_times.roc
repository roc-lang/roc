# repro for https://github.com/roc-lang/roc/issues/9858
# The default-app platform fixes args as List(Str), so x * 10 must report a
# normal missing-method diagnostic for Str.times.
main! = |args| {
    x = args.get(0)?
    _ = x * 10

    Ok({})
}
